{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module HLearn.Models.Distributions.Moments
    ( MomentsParams (..)
    , Moments (..)
    )
    where

import GHC.TypeLits

import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
--

data MomentsParams prob (n::Nat) = MomentsParams

newtype Moments prob (n::Nat) = Moments (VU.Vector prob)
    deriving (Read,Show)

-------------------------------------------------------------------------------
-- Algebra

instance (Num prob, VU.Unbox prob) => Semigroup (Moments prob n) where
    (Moments ma) <> (Moments mb) = Moments $ VU.zipWith (+) ma mb
    
instance (Num prob, VU.Unbox prob, SingI n) => Monoid (Moments prob n) where
    mappend = (<>)
    mempty = Moments $ VU.replicate (n+1) 0
        where n=fromIntegral $ fromSing (sing :: Sing n)
    
instance (Num prob, VU.Unbox prob) => RegularSemigroup (Moments prob n) where
    inverse (Moments m) = Moments $ VU.map negate m
    
-------------------------------------------------------------------------------
-- Training
    
instance Model (MomentsParams prob n) (Moments prob n) where
    getparams _ = MomentsParams

instance DefaultModel (MomentsParams prob n) (Moments prob n) where
    defparams = MomentsParams

instance (VU.Unbox prob, Fractional prob, SingI n) => HomTrainer (MomentsParams prob n) prob (Moments prob n) where
    train1dp' _ dp = Moments $ VU.fromList [dp^^i | i <- [0..n]]
        where n=fromIntegral $ fromSing (sing :: Sing n)