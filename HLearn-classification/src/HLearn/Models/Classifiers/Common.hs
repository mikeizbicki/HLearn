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

class LabeledAttributes dp where
    type Label dp
    type Attributes dp
    
    getLabel :: dp -> Label dp
    getAttributes :: dp -> Attributes dp

instance LabeledAttributes (label,attr) where
    type Label (label,attr) = label
    type Attributes (label,attr) = attr
    
    getLabel = fst
    getAttributes = snd

class 
    ( Label (Datapoint model) ~ Datapoint (ResultDistribution model)
    , Mean (ResultDistribution model)
    , LabeledAttributes (Datapoint model)
    ) => Classifier model 
        where
    type ResultDistribution model
    
    probabilityClassify :: model -> Attributes (Datapoint model) -> ResultDistribution model
    
    classify :: model -> Attributes (Datapoint model) -> Label (Datapoint model)
    classify model dp = mean $ probabilityClassify model dp

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