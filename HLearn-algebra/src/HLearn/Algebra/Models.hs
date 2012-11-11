{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Models
    ( Labeled
    , Weighted
    , Model (..)
    , Label (..)
    , HomTrainer (..)
    , module Control.DeepSeq
    , module Data.Hashable
    , module Data.Binary
    )
    where
          
import HLearn.Algebra.Structures
import HLearn.Algebra.Functions

import Control.DeepSeq
import Data.Hashable
import Data.Binary

-------------------------------------------------------------------------------
-- Idioms

type Labeled var label  = (label,var)
type Weighted var       = (var,Double)

-- | I only ever expect labels of type Bool, Int, and String, but it may be convenient to use other types as well for something.  This class and instance exist so that we have some reasonable assumptions about what properties labels should have for our other classes to work with.  It also keeps us from writing so many constraints.
class (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label
instance (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

-------------------------------------------------------------------------------
-- Model

-- | Every model has at least one data type that that fully describes its parameters.  Many models do not actually *need* any parameters, in which case they will simply use an empty data type.
class Model modelparams model | modelparams -> model where
    getparams :: model -> modelparams

-- | The batch trainer (i.e. `train`) must be a semigroup homomorphism.
class 
    ( Semigroup model
    , Monoid model
    , Model modelparams model
    ) => HomTrainer modelparams datapoint model | modelparams datapoint -> model
        where

    trainSingle :: modelparams -> datapoint -> model
    trainSingle modelparams = unbatch (train modelparams)
    
    train :: modelparams -> [datapoint] -> model
    train modelparams = batch (trainSingle modelparams)
    
    trainOnline :: model -> datapoint -> model
    trainOnline model = online (trainSingle (getparams model :: modelparams)) model
    
    trainOnlineBatch :: model -> [datapoint] -> model
    trainOnlineBatch model = online (train (getparams model :: modelparams)) model