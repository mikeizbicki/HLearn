{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module HLearn.Models.Distributions.Univariate.Binomial
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

import qualified Statistics.Distribution as S
import Statistics.Distribution.Binomial

-------------------------------------------------------------------------------
-- data types

newtype Binomial sample prob = Binomial {  bmoments :: (Moments3 sample) }
    deriving (Read,Show,Eq,Ord,Monoid,Group)
    
-------------------------------------------------------------------------------
-- Training

instance (Num sample) => HomTrainer (Binomial sample prob) where
    type Datapoint (Binomial sample prob) = sample
    train1dp dp = Binomial $ train1dp dp

-------------------------------------------------------------------------------
-- distribution

instance (Num sample) => Probabilistic (Binomial sample prob) where
    type Probability (Binomial sample prob) = prob
    
instance (Floating prob) => PDF (Binomial Int Double) where
    pdf (Binomial dist) dp = S.probability (binomial n p) dp
        where
            n = bin_n $ Binomial dist
            p = bin_p $ Binomial dist

bin_n :: Binomial Int Double -> Int
bin_n (Binomial dist) = round $ ((fromIntegral $ m1 dist :: Double) / (fromIntegral $ m0 dist)) / (bin_p $ Binomial dist)

bin_p :: Binomial Int Double -> Double
bin_p (Binomial dist) = ((fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)) + 1 - (fromIntegral $ m2 dist)/(fromIntegral $ m1 dist)

instance 
    ( PDF (Binomial sample prob)
--     , PlottableDataPoint sample
    , Show prob
    , Show sample
    , Ord sample
    , Ord prob
    , Num prob
    , Integral sample
    ) => PlottableDistribution (Binomial sample prob) 
-- instance PlottableDistribution (Poisson Int Double) 
        where

    plotType _ = Points

    samplePoints dist = [min..max]
        where
            min = 0
            max = maximum [20,floor $ 3*mu]
            mu = 5
    
-- instance (Fractional prob) => Mean (Binomial prob) where
--     mean (Binomial dist) = 
-- 
-- instance (Fractional prob) => Variance (Binomiral prob) where
--     variance dist = mean dist
