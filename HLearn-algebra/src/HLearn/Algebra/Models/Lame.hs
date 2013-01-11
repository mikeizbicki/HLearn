{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Lame trainers are trainers that are crippled---They are not instances of Semigroup/Monoid, and training their models is not a homomorphism.  This means we can't do any of the cool manipulations automatically that we can do with the HomTrainer class.

module HLearn.Algebra.Models.Lame
    ( 
    -- * Classes
    LameTrainer (..)
    , LameTrainerOnline (..)
    , Sizable (..)
    )
    where
          
import qualified Control.ConstraintKinds as CK
import HLearn.Algebra.Models

class Sizable t where
    size :: t a -> Int
    
instance Sizable [] where
    size = length
          
class (Model modelparams model) => LameTrainer modelparams datapoint model where
    lame_train :: 
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , Sizable container
        ) => modelparams -> container datapoint -> model
    
class (Model modelparams model) => LameTrainerOnline modelparams datapoint model where
    lame_add1dp :: model -> datapoint -> model