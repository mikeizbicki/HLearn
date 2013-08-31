

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Marginalization is a tricky procedure involving a lot of type hackery.  All user-facing interfaces use the "Marginalize" class, and all internal interfaces use the "Marginalize'" class.  Essentially, "TypeLens" class converts the "Type lens" for our user's data type into a type level number.  The  "Marginalize'" class then takes this type number and uses it as an index into an appropriate type list that represents the data type.
--
-- The TemplateHaskell module has more info.

module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    where

import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.TypeLens

-------------------------------------------------------------------------------
-- external type classes

class (Marginalize' (TypeLensIndex index) dist, TypeLens index) => Marginalize index dist where

    type Margin index dist 
    getMargin :: index -> dist -> Margin index dist
    
    type MarginalizeOut index dist 
    marginalizeOut :: index -> dist -> MarginalizeOut index dist
    
    condition :: index -> Datapoint (Margin' (TypeLensIndex index) dist) -> dist -> MarginalizeOut' (TypeLensIndex index) dist

instance 
    ( Marginalize' (TypeLensIndex index) dist
    , TypeLens index
    ) => Marginalize index dist 
        where
    
    type Margin index dist = Margin' (TypeLensIndex index) dist
    getMargin _ dist = getMargin' (undefined :: (TypeLensIndex index)) dist
    
    type MarginalizeOut index dist = MarginalizeOut' (TypeLensIndex index) dist
    marginalizeOut _ dist = marginalizeOut' (undefined :: (TypeLensIndex index)) dist
    condition _ dp dist = condition' (undefined :: (TypeLensIndex index)) dist dp

-------------------------------------------------------------------------------
-- internal type classes

class Marginalize' index dist where
    type Margin' index dist
    getMargin' :: index -> dist -> Margin' index dist
    
    type MarginalizeOut' index dist 
    marginalizeOut' :: index -> dist -> MarginalizeOut' index dist
    
    condition' :: index -> dist -> Datapoint (Margin' index dist) -> MarginalizeOut' index dist
    
--     conditionAllButOne :: index -> dist -> Datapoint dist -> MarginalizeOut index dist

