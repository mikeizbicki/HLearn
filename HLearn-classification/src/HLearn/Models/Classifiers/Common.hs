{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE OverlappingInstances #-}

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
-- Labeled datapoints

class Labeled dp where
    type Label dp
    type Attributes dp
    
    getLabel :: dp -> Label dp
    getAttributes :: dp -> Attributes dp

instance Labeled (label,attr) where
    type Label (label,attr) = label
    type Attributes (label,attr) = attr
    
    getLabel = fst
    getAttributes = snd

-------------------------------------------------------------------------------
-- Classification

class 
    ( Labeled (Datapoint model)
    ) => ProbabilityClassifier model 
        where
    type ResultDistribution model    
    probabilityClassify :: model -> Attributes (Datapoint model) -> ResultDistribution model
    
class MarginClassifier model where
    margin :: model -> Attributes (Datapoint model) -> (Ring model, Label (Datapoint model))
    
class 
    ( Labeled (Datapoint model)
    ) => Classifier model
        where
    classify :: model -> Attributes (Datapoint model) -> Label (Datapoint model)

-- | this is a default instance that any instance of Classifier should satisfy if it is also an instance of ProbabilityClassifier
-- instance 
--     ( Label (Datapoint model) ~ Datapoint (ResultDistribution model)
--     , Mean (ResultDistribution model)
--     , ProbabilityClassifier model
--     ) => Classifier model
--         where
--     classify model dp = mean $ probabilityClassify model dp

-------------------------------------------------------------------------------
-- Regression

-- | Regression is classification where the class labels are (isomorphic to) real numbers.  The constraints could probably be better specified, but they're close enough for now.
class (Classifier model, Ring model ~ Label (Datapoint model)) => Regression model
instance (Classifier model, Ring model ~ Label (Datapoint model)) => Regression model
