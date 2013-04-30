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
{-# LANGUAGE DataKinds #-}

module HLearn.Algebra.Structures.Free.Bagging
    ( Bagging
    , Bagging'
    )
    where

import Data.Hashable
import qualified Data.Vector as V
import GHC.TypeLits
import System.Random

import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- data types

newtype Bagging' (n::Nat) (seed::Nat) model = Bagging'
    { modelL :: V.Vector model
    }
    deriving (Read,Show,Eq,Ord)

type Bagging n model = Bagging' n 0 model

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian model, SingI n) => Abelian (Bagging' n seed model) where
instance (Monoid model, SingI n) => Monoid (Bagging' n seed model) where
    mempty = Bagging' $ V.replicate (fromIntegral $ fromSing $ (sing :: Sing n)) mempty
    (Bagging' v1) `mappend` (Bagging' v2) = Bagging' $ V.zipWith (<>) v1 v2

instance (Group model, SingI n) => Group (Bagging' n seed model) where
    inverse (Bagging' v) = Bagging' $ fmap inverse v

instance (HasRing model) => HasRing (Bagging' n seed model) where
    type Ring (Bagging' n seed model) = Ring model

instance (Module model, SingI n) => Module (Bagging' n seed model) where
    r .* (Bagging' v) = Bagging' $ fmap (r.*) v

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer model
    , SingI n
    , SingI seed
    , Hashable (Datapoint model)
    ) => HomTrainer (Bagging' n seed model) 
        where
    
    type Datapoint (Bagging' n seed model) = Datapoint model
    
    train1dp dp = Bagging' $ V.replicate n mempty V.// [(offset `mod` n,train1dp dp)]
        where
            n = fromIntegral $ fromSing (sing :: Sing n)
            seed = fromIntegral $ fromSing (sing :: Sing seed)
            (offset,g) = random $ mkStdGen $ hash dp + seed