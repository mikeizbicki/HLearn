{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module HLearn.Algebra.Structures.Free.FreeHomTrainer
    ( FreeHomTrainer
    , FreeHomTrainer'
    )
    where

import Control.Applicative
import qualified Data.Map as Map

import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Models.Lame
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Free.FreeModule

-------------------------------------------------------------------------------
-- data types

newtype FreeHomTrainer' container model = FreeHomTrainer'
    { modelL :: container model
    }
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian)

type family FreeHomTrainer (model:: *) (algebra::a) (merge::b) :: x
type instance FreeHomTrainer model Monoid        NoFlatten = FreeHomTrainer' FreeMonoid model
type instance FreeHomTrainer model Group         NoFlatten = FreeHomTrainer' FreeGroup model
type instance FreeHomTrainer model AbelianGroup  NoFlatten = FreeHomTrainer' (FreeModule Int) model
type instance FreeHomTrainer model (Module ring) NoFlatten = FreeHomTrainer' (FreeModule ring) model

data NoFlatten

newtype FreeMonoid a = FreeMonoid [a]
newtype FreeGroup a = FreeGroup [a]
data AbelianGroup 

-------------------------------------------------------------------------------
-- Algebra

instance (HasRing (container model)) => HasRing (FreeHomTrainer' container model) where
    type Ring (FreeHomTrainer' container model) = Ring (container model)
    
deriving instance (Module (container model)) => Module (FreeHomTrainer' container model)

-------------------------------------------------------------------------------
-- Training

instance 
    ( Num ring
    , Ord model
    , LameTrainer model
    , Applicative container
    , Monoid (container model)
    ) => HomTrainer (FreeHomTrainer' container model) 
        where
    
    type Datapoint (FreeHomTrainer' container model) = (LameContainer model) (LameDatapoint model)
    
    train1dp dp = FreeHomTrainer'
        { modelL = pure $ lame_train dp
        }