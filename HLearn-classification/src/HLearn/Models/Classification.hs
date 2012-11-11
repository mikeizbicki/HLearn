{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts, BangPatterns #-}

module HLearn.Models.Classification
    where

import Data.Number.LogFloat

import HLearn.Algebra
import HLearn.Models.Distributions.Categorical

-------------------------------------------------------------------------------
-- Classification

class (Label label) => ProbabilityClassifier model datatype label | model -> label where
    probabilityClassify :: model -> datatype -> Categorical label Double
    
--     straightClassify :: model -> datatype -> label
--     straightClassify = mean . probabilityClassify
--     straightClassify model dp = classificationLabel $ probabilityClassify model dp
--     straightClassify model dp = fst . argmaxBy compare snd $ probabilityClassify model dp
    
class (Label label) => Classifier model datatype label | model -> label where
    classify :: model -> datatype -> label

-- instance (ProbabilityClassifier model datatype label) => Classifier model datatype label where
--     classify model dp = mean $ probabilityClassify model dp