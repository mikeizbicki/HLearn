{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

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

instance Model (NoParams (Unital prob)) (Unital prob) where
    getparams _ = NoParams
    
instance DefaultModel (NoParams (Unital prob)) (Unital prob) where
    defparams = NoParams
    
instance (Num prob) => HomTrainer (NoParams (Unital prob)) () (Unital prob) where
    train1dp' _ () = Unital 1
    
-------------------------------------------------------------------------------
-- distributions

instance Distribution (Unital prob) () prob where
    pdf (Unital prob) () = prob

