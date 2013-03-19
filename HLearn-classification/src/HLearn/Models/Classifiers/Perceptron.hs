{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module HLearn.Models.Classifiers.Perceptron
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification
import HLearn.Models.Classifiers.NearestNeighbor

-------------------------------------------------------------------------------
-- data structures

data Perceptron label weight = Perceptron 
    { vecL :: Map.Map label (Centroid weight)
    }

type Centroid weight = (weight,VU.Vector Double)

-------------------------------------------------------------------------------
-- algebra

