{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Models.Classifiers.Common
    where

import HLearn.Algebra
import HLearn.Models.Distributions

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
-- Classification

class Classifier model where
    type Label model
    type UnlabeledDatapoint model
    
    probabilityClassify :: model -> UnlabeledDatapoint model -> Categorical (Label model) Double
    
    classify :: model -> UnlabeledDatapoint model -> Label model
    classify model dp = mostLikely $ (probabilityClassify model dp :: Categorical (Label model) Double)
    

{-class (Ord prob) => ProbabilityClassifier model datatype label prob | model -> label prob where
    probabilityClassify :: model -> datatype -> Categorical label prob
    classify :: model -> datatype -> label
    classify model dp = mostLikely $ (probabilityClassify model dp :: Categorical label prob)
    -}
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