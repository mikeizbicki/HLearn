{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.Algebra.Structures.Free.FreeHomTrainer
    ( FreeHomTrainer
    , FreeHomTrainer'
    )
    where

import qualified Data.Map as Map

import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Models.Lame
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Free.FreeModule

-------------------------------------------------------------------------------
-- data types

newtype FreeHomTrainer' ring model = FreeHomTrainer'
    { modelL :: FreeModule ring model
    }
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian,Module)

type FreeHomTrainer model = FreeHomTrainer' (Ring model) model

-------------------------------------------------------------------------------
-- Algebra

instance (Num ring) => HasRing (FreeHomTrainer' ring model) where
    type Ring (FreeHomTrainer' ring model) = Ring (FreeModule ring model)
    
-------------------------------------------------------------------------------
-- Training

instance 
    ( Num ring
    , Ord model
    , LameTrainer model
    ) => HomTrainer (FreeHomTrainer' ring model) 
        where
    
    type Datapoint (FreeHomTrainer' ring model) = (LameContainer model) (LameDatapoint model)
    
    train1dp dp = FreeHomTrainer'
        { modelL = FreeModule $ Map.singleton (lame_train dp) 1
        }