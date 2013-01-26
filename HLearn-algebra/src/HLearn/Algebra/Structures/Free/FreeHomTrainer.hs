{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module HLearn.Algebra.Structures.Free.FreeHomTrainer
    where

import qualified Control.ConstraintKinds as CK

import HLearn.Algebra.Models
import HLearn.Algebra.Models.Lame
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Free.RegSG2Group

-------------------------------------------------------------------------------
-- FHomTrainerParams

data FHomTrainerParams' (container :: * -> *) params = FHomTrainerParams' 
    { baseparams :: params
    }
    deriving (Read,Show,Eq,Ord)
    
type FHomTrainerParams params = FHomTrainerParams' [] params
    
data FHomTrainer' container params model = FHomTrainer'
    { params :: FHomTrainerParams' container params
    , modelL :: container model
    }
    deriving (Read,Show,Eq,Ord)

type FHomTrainerC container params model = RegSG2Group (FHomTrainer' container params model)
type FHomTrainer params model = FHomTrainerC [] params model

instance Model (FHomTrainerParams' container params) (FHomTrainerC container params model) where
    getparams (SGJust model) = params model
    
instance (DefaultModel params model) => 
    DefaultModel 
        (FHomTrainerParams' container params) 
        (FHomTrainerC container params model) 
            where
    defparams = FHomTrainerParams' defparams

-------------------------------------------------------------------------------
-- Algebra

instance (Eq params, Semigroup (container model)) => Semigroup (FHomTrainer' container params model) where
    ht1 <> ht2 = if params ht1 /= params ht2
        then error "FHomTrainer.(<>): params not equal"
        else ht1 { modelL = (modelL ht1) <> (modelL ht2) }
        
-------------------------------------------------------------------------------
-- Training

instance 
    ( Semigroup (htcontainer model)
    , CK.Applicative htcontainer
    , CK.ApplicativeConstraint htcontainer model
    , LameTrainer params dpcontainer dp model
    , Eq params
    ) => 
    HomTrainer 
        (FHomTrainerParams' htcontainer params) 
        (dpcontainer dp)
        (FHomTrainerC htcontainer params model) 
            where
                  
    train1dp' params dp = SGJust $ FHomTrainer'
        { params = undefined
        , modelL = CK.pure $ lame_train' (baseparams params) dp
        }