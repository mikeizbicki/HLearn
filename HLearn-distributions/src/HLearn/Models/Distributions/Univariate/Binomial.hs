{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


module HLearn.Models.Distributions.Categorical
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data Binomial dp = Binomial
    { m0 :: Rational
    , m1 :: Rational
    }

-------------------------------------------------------------------------------
-- algebra

instance Semigroup (Binomial dp) where
    b1 <> b2 = undefined
    
instance Monoid (Binomial dp) where
    mempty = Binomial 0 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- training

instance Model (NoParams (Binomial dp)) (Binomial dp) where
    getparams _ = NoParams
    
instance DefaultModel (NoParams (Binomial dp)) (Binomial dp) where
    defparams = NoParams
    
instance (Integral dp) => HomTrainer (NoParams (Binomial dp)) dp (Binomial dp) where
    train1dp' _ dp = Binomial
        { m0 = 1
        , m1 = fromIntegral dp
        }

-------------------------------------------------------------------------------
-- distribution