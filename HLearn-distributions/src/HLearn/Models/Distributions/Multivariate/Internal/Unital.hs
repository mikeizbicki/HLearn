{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module HLearn.Models.Distributions.Multivariate.Internal.Unital
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types

newtype Unital prob = Unital prob
    deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- algebra

instance (Num prob) => Monoid (Unital prob) where
    mempty = Unital 0
    (Unital p1) `mappend` (Unital p2) = Unital $ p1+p2

instance (Num prob) => Group (Unital prob) where
    inverse (Unital p) = Unital (-p)

-------------------------------------------------------------------------------
-- training

instance (Num prob) => HomTrainer (Unital prob) where
    type Datapoint (Unital prob) = HList '[]
    train1dp HNil = Unital 1
    
instance NumDP (Unital prob) prob where
    numdp (Unital prob) = prob
    
-------------------------------------------------------------------------------
-- distributions

instance (Num prob) => Probabilistic (Unital prob) where
    type Probability (Unital prob) = prob

instance (Num prob) => PDF (Unital prob) where
    pdf (Unital prob) HNil = 1 --prob

