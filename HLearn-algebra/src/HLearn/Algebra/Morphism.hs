{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Morphism
    ( Morphism (..)
    , MorphismComposition (..)
    )
    where

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra.Models
import HLearn.Algebra.Structures.Groups

class Morphism domain params codomain | params -> codomain where
    morph :: domain -> params -> codomain
    morph = ($>)

    ($>) :: domain -> params -> codomain
    ($>) = morph
        
    (<.>) :: params -> domain -> codomain
    (<.>) = flip morph

data 
    ( Morphism domain params1 interdomain
    , Morphism interdomain params2 codomain
    ) => MorphismComposition domain params1 interdomain params2 codomain = (:.) params2 params1

instance 
    ( Morphism domain params1 interdomain
    , Morphism interdomain params2 codomain
    ) => Morphism domain (MorphismComposition domain params1 interdomain params2 codomain) codomain
    where
        morph x (params2 :. params1) = morph (morph x params1) params2

-------------------------------------------------------------------------------

-- data HLearn params domain codomain = HLearn params
-- instance Category (MorphismComposition' params1 interdomain params2) where
--     id a = a
--     (.) = 

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

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer modelparams datapoint model
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , CK.Functor container
    , CK.FunctorConstraint container datapoint
    , CK.FunctorConstraint container model
    ) => Morphism (container datapoint) modelparams model
    where
        morph input params = train' params input
    
instance 
    ( Model params1 interdomain
    , Model params2 codomain
    ) => Model (MorphismComposition domain params1 interdomain params2 codomain) codomain
    where
        getparams model = undefined

-- instance 
--     ( DefaultModel params1 interdomain
--     , DefaultModel params2 codomain
--     ) => DefaultModel (MorphismComposition domain params1 interdomain params2 codomain) codomain
--     where
--         defparams = undefined

instance 
    ( CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.FoldableConstraint container model
    , CK.Foldable container
    , CK.Functor container
    , Model (MorphismComposition (container datapoint) params1 interdomain params2 codomain) codomain
    , HomTrainer params1 datapoint interdomain
    , Morphism (container datapoint) params1 interdomain
    , Morphism interdomain params2 codomain
    , Monoid codomain
    , Semigroup codomain
    , Model (MorphismComposition domain params1 interdomain params2 codomain) codomain
    ) => HomTrainer (MorphismComposition (container datapoint) params1 interdomain params2 codomain) datapoint codomain
    where
         train1dp' (params2 :. params1) dp = params2 <.> (train1dp' params1 dp)
