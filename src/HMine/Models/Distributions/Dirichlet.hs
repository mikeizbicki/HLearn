{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Models.Distributions.Dirichlet
    where

import Control.DeepSeq
import Data.List.Extras
import Data.Number.LogFloat
import Data.Semigroup
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Foldable as F

import HMine.Base
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.DataContainers

-------------------------------------------------------------------------------
-- DirichletParams

data DirichletParams = DirichletParams
    deriving (Read,Show,Eq)

instance NFData DirichletParams where
    rnf x = ()

-------------------------------------------------------------------------------
-- Dirichlet

data Dirichlet label = Dirichlet 
        { pdfmap :: Map.Map label Int
        , desc :: Maybe (DataDesc label)
        } 
    deriving (Show,Read,Eq)

instance (NFData label) => NFData (Dirichlet label) where
    rnf d = rnf $ pdfmap d

-------------------------------------------------------------------------------
-- Distribution

instance (Ord label) => Distribution (Dirichlet label) label where
    
    {-# INLINE add1sample #-}
    add1sample dist label = dist { pdfmap=Map.insertWith (+) label 1 (pdfmap dist) }

    {-# INLINE pdf #-}
    pdf dist label = logFloat $ 0.0001+((fi val)/(fi tot)::Double)
        where
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> 0
                Just x  -> x
            tot = F.foldl' (+) 0 $ pdfmap dist

    {-# INLINE cdf #-}
    cdf dist label = (fi $ Map.foldl' (+) 0 $ Map.filterWithKey (\k a -> k<=label) $ pdfmap dist) 
                   / (fi $ Map.foldl' (+) 0 $ pdfmap dist)
                   
    cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist
    
-------------------------------------------------------------------------------
-- Algebra

instance (Label label) => Semigroup (Dirichlet label) where
    (<>) d1 d2 = if (desc d1)==(desc d2)
        then d1 {pdfmap=Map.unionWith (+) (pdfmap d1) (pdfmap d2)}
        else error "Dirichlet.(<>): different DataDesc"
    
instance (Label label) => Monoid (Dirichlet label) where
    mempty = Dirichlet mempty Nothing
    mappend = (<>)

instance (Label label) => Invertible (Dirichlet label) where
    inverse d1 = d1 {pdfmap=Map.map (0-) (pdfmap d1)}


-------------------------------------------------------------------------------
-- Training

instance (OnlineTrainer DirichletParams (Dirichlet label) datatype label) => 
    BatchTrainer DirichletParams (Dirichlet label) datatype label 
        where
              
    trainBatch = trainOnline

instance (Label label) => EmptyTrainer DirichletParams (Dirichlet label) label where
    emptyModel desc modelparams = Dirichlet Map.empty (Just desc)

instance (Label label) => OnlineTrainer DirichletParams (Dirichlet label) datatype label where
    add1dp desc modelparams model dps = return $ add1sample model $ fst dps

-------------------------------------------------------------------------------
-- Classification

instance (Label label) => Classifier (Dirichlet label) datatype label where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance (Label label) => ProbabilityClassifier (Dirichlet label) datatype label where
    probabilityClassify model dp = --trace "DirichletPC" $
        case Map.keys $ pdfmap model of
            [] -> trace "WARNING: ProbabilityClassifier: empty Dirichlet" $ 
                case desc model of
                    Nothing -> error "probabilityClassify: empty Dirichlet and empty DataDesc"
                    Just desc -> map (\label -> (label,1/(fromIntegral $ numLabels desc))) $ labelL desc
            xs -> map (\k -> (k,pdf model k)) xs
