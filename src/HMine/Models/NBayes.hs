{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances, BangPatterns, FlexibleInstances, UndecidableInstances #-}

module HMine.Models.NBayes
    ( NBayes (..)
    , NBayesParams (..), defNBayesParams
--     , file2nbayes, nbayes2file
    , getDist, labelProb
    )
    where
         
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST.Strict
import Control.Monad.Primitive
import Data.Binary
import Data.List
import Data.List.Extras
import Data.Semigroup
import Data.Number.LogFloat
import Data.STRef
-- import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import Debug.Trace
-- import qualified Numeric.Algebra as Alg
import System.IO
import Test.QuickCheck

import HMine.Base
import HMine.DataContainers
import HMine.Distribution
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.MiscUtils
import HMine.Models.Distributions.Dirichlet

-------------------------------------------------------------------------------
-- NBayesParams

data NBayesParams = NBayesParams
    deriving (Read,Show,Eq)

instance NFData NBayesParams where
    rnf params = ()

defNBayesParams = NBayesParams

-------------------------------------------------------------------------------
-- NBayes

data NBayes label = NBayes
    { dataDesc :: !(DataDesc label)
    , labelDist :: !(Dirichlet label)
    , attrDist :: !(V.Vector (V.Vector DistContainer)) -- ^ The inner vector corresponds to attributes and the outer vector labels
    }
    deriving (Read,Show,Eq)
    
getDist :: NBayes Int -> Int -> Int -> DistContainer
getDist nb attrI label = (attrDist nb) V.! label V.! attrI
    
labelProb :: NBayes Int -> Int -> LogFloat
labelProb = pdf . labelDist
    
instance Model (NBayes label) label where
    datadesc = dataDesc

instance (NFData label) => NFData (NBayes label) where
    rnf nb = seq (rnf $ attrDist nb) $ seq (rnf $ dataDesc nb) (rnf $ labelDist nb)

-------------------------------------------------------------------------------
-- Algebra

instance (Label label) => Invertible (NBayes label) where
    inverse nb = nb
        { labelDist = inverse $ labelDist nb
        , attrDist = V.map (V.map inverse) $ attrDist nb
        }

instance (Label label) => Semigroup (NBayes label) where
--     (<>) a NBayesUndefined = a
--     (<>) NBayesUndefined b = b
    (<>) a b =
        if (dataDesc a)/=(dataDesc b)
           then error $ "mappend.NBayes: cannot combine nbayes with different sizes! lhs="++(show $ dataDesc a)++"; rhs="++(show $ dataDesc b)
           else NBayes
                    { dataDesc = dataDesc a
                    , labelDist = (labelDist a) <> (labelDist b)
                    , attrDist = V.zipWith (V.zipWith mappend) (attrDist a) (attrDist b)
                    }

-------------------------------------------------------------------------------
-- Training

instance (OnlineTrainer NBayesParams (NBayes label) datatype label) => 
    BatchTrainer NBayesParams (NBayes label) datatype label 
        where
              
    trainBatch = trainOnline

instance (Label label) => EmptyTrainer NBayesParams (NBayes label) label where
    emptyModel desc NBayesParams = NBayes
        { dataDesc = desc
        , labelDist = mempty
        , attrDist = V.fromList [V.fromList [mempty | y<-[1..numAttr desc]] | x<-[1..numLabels desc]]
        }

instance OnlineTrainer NBayesParams (NBayes Int) DPS Int where

--     add1dp desc NBayesUndefined (label,dp) = add1dp desc (emptyNBayes desc) (label,dp)
    add1dp desc modelparams nb (label,dp) = return $
        nb  { labelDist = add1sample (labelDist nb) label
            , attrDist = (attrDist nb) V.// [(label,newLabelVec)] 
            }
        where
            newLabelVec = V.accum add1sample (attrDist nb V.! label) dp
    

-------------------------------------------------------------------------------
-- Classification

instance Classifier (NBayes Int) DPS Int where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance ProbabilityClassifier (NBayes Int) DPS Int where
    probabilityClassify nb dp =  normedAnswer
        where
            labelProbGivenDp label = (labelProbGivenNothing label)*(dpProbGivenLabel label)
            labelProbGivenNothing label = pdf (labelDist nb) label
            dpProbGivenLabel label = foldl (*) (logFloat (1::Double)) (attrProbL label)
            attrProbL label = [ pdf (attrDist nb V.! label V.! attrIndex) di | (attrIndex,di) <- dp]

            answer = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
            normedAnswer = zip [0..] $ normalizeL [labelProbGivenDp label | label <- [0..(numLabels $ dataDesc nb)-1]]

-------------------------------------------------------------------------------
-- PartialBayes

data PartialBayes label = PartialBayes 
    { nbayes :: (NBayes label)
    , attrL :: [Int]
    }

instance Classifier (PartialBayes Int) DPS Int where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance ProbabilityClassifier (PartialBayes Int) DPS Int where
    probabilityClassify (PartialBayes nb attrL) dpinput = normedAnswer
        where
            dp = filter (\(attrI,di)->attrI `elem` attrL) dpinput
              
            labelProbGivenDp label = (labelProbGivenNothing label)*(dpProbGivenLabel label)
            labelProbGivenNothing label = pdf (labelDist nb) label
            dpProbGivenLabel label = foldl (*) (logFloat (1::Double)) (attrProbL label)
            attrProbL label = [ pdf (attrDist nb V.! label V.! attrIndex) di | (attrIndex,di) <- dp]

            answer = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
            normedAnswer = zip [0..] $ normalizeL [labelProbGivenDp label | label <- [0..(numLabels $ dataDesc nb)-1]]
    