{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module HLearn.Models.Distributions.Unital
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types

newtype Unital prob = Unital prob
    deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- algebra

instance (Num prob) => Semigroup (Unital prob) where
    (Unital p1) <> (Unital p2) = Unital $ p1+p2
    
instance (Num prob) => Monoid (Unital prob) where
    mempty = Unital 0
    mappend = (<>)

instance (Num prob) => RegularSemigroup (Unital prob) where
    inverse (Unital p) = Unital (-p)

-------------------------------------------------------------------------------
-- training

instance ModelParams (Unital prob) where
    type Params (Unital prob) = NoParams
    getparams _ = NoParams
    
    
instance (Num prob) => HomTrainer (Unital prob) where
    type Datapoint (Unital prob) = HList '[]
    train1dp' _ HNil = Unital 1
    
-------------------------------------------------------------------------------
-- distributions

instance PDF (Unital prob) () prob where
    pdf (Unital prob) () = prob

