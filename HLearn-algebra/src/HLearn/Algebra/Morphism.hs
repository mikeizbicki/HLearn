{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Morphism
    ( Morphism (..)
    , MorphismComposition (..)
    )
    where

import HLearn.Algebra.Models
import HLearn.Algebra.Structures

class Morphism domain params codomain | params -> codomain where
    morph :: domain -> params -> codomain
    morph = ($>)
    
    ($>) :: domain -> params -> codomain
    ($>) = morph

data 
    ( Morphism domain params1 interdomain
    , Morphism interdomain params2 codomain
    ) => MorphismComposition domain params1 interdomain params2 codomain = (:.) params1 params2

instance 
    ( Morphism domain params1 interdomain
    , Morphism interdomain params2 codomain
    ) => Morphism domain (MorphismComposition domain params1 interdomain params2 codomain) codomain

-------------------------------------------------------------------------------
-- Properties

class (Morphism domain params codomain) => Surjective domain params codomain
instance 
    ( Surjective domain params1 interdomain
    , Surjective interdomain params2 codomain
    ) => Surjective domain (MorphismComposition domain params1 interdomain params2 codomain) codomain
    
class (Morphism domain params codomain) => Injective domain params codomain
instance 
    ( Injective domain params1 interdomain
    , Injective interdomain params2 codomain
    ) => Injective domain (MorphismComposition domain params1 interdomain params2 codomain) codomain
    
class (Morphism domain params codomain) => Homomorphism domain params codomain
instance 
    ( Homomorphism domain params1 interdomain
    , Homomorphism interdomain params2 codomain
    ) => Homomorphism domain (MorphismComposition domain params1 interdomain params2 codomain) codomain

instance 
    ( HomTrainer params1 datapoint interdomain
    , Morphism domain params1 interdomain
    , Morphism interdomain params2 codomain
    , Monoid codomain
    , Semigroup codomain
    , Model (MorphismComposition domain params1 interdomain params2 codomain) codomain
    ) => HomTrainer (MorphismComposition domain params1 interdomain params2 codomain) datapoint codomain
    where
         train1dp' ((:.) params1 params2) dp = (train1dp' params1 dp) $> params2
