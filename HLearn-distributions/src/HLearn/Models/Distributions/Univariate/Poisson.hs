{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Poisson
    ( Poisson
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import qualified Statistics.Distribution as S
import Statistics.Distribution.Binomial
import Statistics.Distribution.Poisson

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Gnuplot
import HLearn.Models.Distributions.Univariate.Internal.Moments

-------------------------------------------------------------------------------
-- Poisson

newtype Poisson sample prob = Poisson {  pmoments :: (Moments3 sample) }
    deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)

instance (Num sample) => HomTrainer (Poisson sample prob) where
    type Datapoint (Poisson sample prob) = sample
    train1dp dp = Poisson $ train1dp dp

instance (Num sample) => Distribution (Poisson sample prob) where
    type Probability (Poisson sample prob) = prob

-- instance (Integral sample, Floating prob) => PDF (Poisson sample prob) where
--     pdf (Poisson dist) dp 
--         | dp < 0    = 0
--         | dp > 100  = pdf (Normal $ Moments3 (fromIntegral $ m0 dist) (fromIntegral $ m1 dist) (fromIntegral $ m2 dist)) $ fromIntegral dp
--         | otherwise = (lambda^^dp)*(exp $ (-1)*lambda)/(fromIntegral $ factorial dp)
--         where
--             lambda = (fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)
-- factorial :: (Integral a) => a -> a
-- factorial 0 = 1
-- factorial n = n*(factorial $ n-1)

instance (Integral sample, Floating prob) => PDF (Poisson sample Double) where
    pdf (Poisson dist) dp = S.probability (poisson lambda) $ fromIntegral dp
        where
            lambda = (fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)

instance 
    ( PDF (Poisson sample prob)
--     , PlottableDataPoint sample
    , Show prob
    , Show sample
    , Ord sample
    , Ord prob
    , Fractional prob
    , Integral sample
    ) => PlottableDistribution (Poisson sample prob) 
-- instance PlottableDistribution (Poisson Int Double) 
        where

    plotType _ = Points

    samplePoints dist = [min..max]
        where
            min = 0
            max = maximum [20,floor $ 3*lambda]
            lambda = (fromIntegral $ m1 $ pmoments dist) / (fromIntegral $ m0 $ pmoments dist)
    
-- instance (Fractional prob) => Mean (Poisson sample prob) where
--     mean (Poisson dist) = (fromIntegral $ m1 $ pmoments dist) / (fromIntegral $ m0 $ pmoments dist)
-- 
-- instance (Fractional prob) => Variance (Poisson sample prob) where
--     variance dist = mean dist
