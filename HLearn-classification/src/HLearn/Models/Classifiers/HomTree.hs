{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module HLearn.Models.Classifiers.HomTree
    where

import Control.DeepSeq
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Vector as V
import qualified Control.ConstraintKinds as CK

import HLearn.Algebra
import HLearn.DataContainers
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.NBayes
import HLearn.Models.Classification

fi=fromIntegral

-------------------------------------------------------------------------------
-- HomTreeParams

data HomTreeParams = HomTreeParams
    {
    }
    deriving (Show, Read, Eq,Ord)
    
defHomTreeParams = HomTreeParams

instance NFData HomTreeParams where
    rnf params = ()
    
instance (Eq label) => Model (ClassificationParams label HomTreeParams) (HomTree label) where
    getparams _ = error "HomTree.getparams"
    
-------------------------------------------------------------------------------
-- HomTree

data HomTree' label = HomTree'
    { nbayes :: !(NBayes label)
    , splitAttr :: !Int
    }

instance (NFData (NBayes label)) => NFData (HomTree' label) where
    rnf (HomTree' nbayes splitAttr) = deepseq nbayes $ rnf splitAttr

type HomTree label = RegSG2Group (HomTree' label)

-------------------------------------------------------------------------------
-- Functors

nbayes2homtree :: NaiveBayes Int -> HomTree' Int
nbayes2homtree (SGJust nbayes) = HomTree'
    { nbayes = nbayes
    , splitAttr = getSplitAttr nbayes
--     , splitAttr = error "nbayes2homtree: not implemented"
    }
    
getSplitAttr nbayes = argmax (attrEntropy nbayes) $ attrL $ dataDesc nbayes
    
-- attrEntropy :: (ProbabilityClassifier (NBayes label) DPS label) => NBayes label -> Int -> HLearn Double
attrEntropy :: NBayes Int -> Int -> Double
attrEntropy nb attrI = nonoverlap $ V.toList $ V.map (V.! attrI) $ attrDist nb
--      do
-- --     let labelL = [0,1,0,1]
--     let labelL = (map (cdfInverse (labelDist nb) {-. logFloat-})) $ map (/100) [1..99]
--     let sampleL = map (cdfInverse . getDist nb attrI) labelL
--     let pcL = map (\sample -> probabilityClassify nb [(attrI,sample)]) sampleL
--     let pcLg = groupBy (\pc1 pc2 -> (mean pc1)==(mean pc2)) pcL
--     let weightL = normalizeL $ map (fi . length) pcLg
--     let entropyL = map (average . map (classificationEntropy . dist2list)) pcLg
--     let entropy = sum $ map (\(w,e) -> w*e) $ zip weightL entropyL
--     return entropy

average :: [Double] -> Double
average xs = (sum xs) / (fi $ length xs)

-------------------------------------------------------------------------------
-- Algebra

-- instance (Label label, Semigroup (NBayes label)) => Semigroup (HomTree label) where
instance Semigroup (HomTree' Int) where
    (<>) homtree1 homtree2 = nbayes2homtree $ SGJust $ ((nbayes homtree1) <> (nbayes homtree2))
--     (<>) homtree1 homtree2 = HomTree' ((nbayes homtree1) <> (nbayes homtree2)) Nothing

-- instance (Label label, Invertible (NBayes label)) => Invertible (HomTree label) where
instance RegularSemigroup (HomTree' Int) where
    inverse homtree = nbayes2homtree $ SGJust $ (inverse (nbayes homtree))
--     inverse homtree = HomTree' (inverse (nbayes homtree)) Nothing

-------------------------------------------------------------------------------
-- Training

instance 
--     ( HomTrainer (ClassificationParams label (NBayesParams label)) (label,DPS) (NaiveBayes label)
--     ) => HomTrainer (ClassificationParams label HomTreeParams) (label,DPS) (HomTree label) 
    ( {-HomTrainer (ClassificationParams Int (NBayesParams Int)) (Int,DPS) (NaiveBayes Int)-}
    ) => HomTrainer (ClassificationParams Int HomTreeParams) (Int,DPS) (HomTree Int) 
        where
    train' (ClassificationParams params datadesc) dp = 
        SGJust $ nbayes2homtree $ deepseq nb nb
        where
            nb = train' (ClassificationParams defNBayesParams datadesc) dp

{-instance BatchTrainer HomTreeParams (HomTree Int) DPS Int where
    trainBatch = trainOnline

instance EmptyTrainer HomTreeParams (HomTree Int) Int where
    emptyModel desc modelparams = HomTree 
        { nbayes = emptyModel desc defNBayesParams
        , splitAttr = trace "WARNING: HomTree.EmptyTrainer: no splitAttr yet" $ 0
        }

instance OnlineTrainer HomTreeParams (HomTree Int) DPS Int where
    add1dp desc modelparams model dp = do
        nbayes' <- add1dp desc defNBayesParams (nbayes model) dp
        return $ nbayes2homtree nbayes'-}
        
-------------------------------------------------------------------------------
-- Classification

-- instance Classifier (HomTree Int) DPS Int where
--     classify model dp = mean $ probabilityClassify model dp

-- instance (Ord prob) => ProbabilityClassifier (HomTree label) DPS label prob where
instance ProbabilityClassifier (HomTree Int) DPS Int Double where
              
    probabilityClassify (SGJust model) dp = probabilityClassify (nbayes model) dp'
        where dp' = filter (\(attrI,val) -> attrI==splitAttr model) dp