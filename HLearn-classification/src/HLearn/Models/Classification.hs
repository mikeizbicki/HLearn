{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts, BangPatterns, ScopedTypeVariables #-}

module HLearn.Models.Classification
    where

import Data.Number.LogFloat

import HLearn.Algebra
import HLearn.Models.Distributions
-- import HLearn.Models.Distributions.Categorical
-- import HLearn.DataContainers


-------------------------------------------------------------------------------
-- bool <-> int

indicator :: (Num a) => Bool -> a
indicator b = 
    if b
        then 1
        else 0                 

bool2num :: (Num a) => Bool -> a
bool2num b = 
    if b
        then 1
        else -1
                 
num2bool :: (Ord a, Num a) => a -> Bool
num2bool a = 
    if a<0
        then False
        else True 

-------------------------------------------------------------------------------
-- ClassificationParams

-- data ClassificationParams label params = ClassificationParams
--     { cparams :: params
--     , datadesc :: DataDesc label
--     }
--     deriving (Read,Show,Eq,Ord)

data DataDesc label = DataDesc
    { numLabels :: Int
    , labelL :: [label]
    , numAttr :: Int
    }
    deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- Classification

class (Ord prob) => ProbabilityClassifier model datatype label prob | model -> label prob where
    probabilityClassify :: model -> datatype -> Categorical label prob
    classify :: model -> datatype -> label
    classify model dp = mostLikely $ (probabilityClassify model dp :: Categorical label prob)
    
instance (ProbabilityClassifier model datatype label prob) => 
    ProbabilityClassifier (Weighted model) datatype label prob 
        where
        
    probabilityClassify (Weighted model) dp = probabilityClassify model dp
    
--     straightClassify :: model -> datatype -> label
--     straightClassify = mean . probabilityClassify
--     straightClassify model dp = classificationLabel $ probabilityClassify model dp
--     straightClassify model dp = fst . argmaxBy compare snd $ probabilityClassify model dp
    
-- class {-(Label label) =>-} Classifier model datatype label | model -> label where
--     classify :: model -> datatype -> label
--     classify model dp = mostLikely $ probabilityClassify model dp
-- 
-- instance (ProbabilityClassifier model datatype label) => Classifier model datatype label where
--     classify model dp = mostLikely $ probabilityClassify model dp