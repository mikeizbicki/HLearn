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

newtype Binomial (prob :: *) dp = Binomial {  bmoments :: (Moments3 dp) }
    deriving (Read,Show,Eq,Ord,Monoid,Group)
    
-------------------------------------------------------------------------------
-- Training

instance (Num dp) => HomTrainer (Binomial prob dp) where
    type Datapoint (Binomial prob dp) = dp
    train1dp dp = Binomial $ train1dp dp

-------------------------------------------------------------------------------
-- distribution

instance (Num dp) => Probabilistic (Binomial prob dp) where
    type Probability (Binomial prob dp) = prob
    
instance (Floating prob) => PDF (Binomial Double Int) where
    pdf (Binomial dist) dp = S.probability (binomial n p) dp
        where
            n = bin_n $ Binomial dist
            p = bin_p $ Binomial dist

bin_n :: Binomial Double Int -> Int
bin_n (Binomial dist) = round $ ((fromIntegral $ m1 dist :: Double) / (fromIntegral $ m0 dist)) / (bin_p $ Binomial dist)

bin_p :: Binomial Double Int -> Double
bin_p (Binomial dist) = ((fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)) + 1 - (fromIntegral $ m2 dist)/(fromIntegral $ m1 dist)

instance 
    ( PDF (Binomial prob dp)
--     , PlottableDataPoint dp
    , Show prob
    , Show dp
    , Ord dp
    , Ord prob
    , Num prob
    , Integral dp
    ) => PlottableDistribution (Binomial prob dp) 
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
