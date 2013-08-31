
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
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Poisson

newtype Poisson prob dp = Poisson {  pmoments :: (Moments3 dp) }
    deriving (Read,Show,Eq,Ord,Monoid,Group)

instance (Num dp) => HomTrainer (Poisson prob dp) where
    type Datapoint (Poisson prob dp) = dp
    train1dp dp = Poisson $ train1dp dp

instance (Num dp) => Probabilistic (Poisson prob dp) where
    type Probability (Poisson prob dp) = prob

-- instance (Integral dp, Floating prob) => PDF (Poisson prob dp) where
--     pdf (Poisson dist) dp 
--         | dp < 0    = 0
--         | dp > 100  = pdf (Normal $ Moments3 (fromIntegral $ m0 dist) (fromIntegral $ m1 dist) (fromIntegral $ m2 dist)) $ fromIntegral dp
--         | otherwise = (lambda^^dp)*(exp $ (-1)*lambda)/(fromIntegral $ factorial dp)
--         where
--             lambda = (fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)
-- factorial :: (Integral a) => a -> a
-- factorial 0 = 1
-- factorial n = n*(factorial $ n-1)

instance (Integral dp, Floating prob) => PDF (Poisson Double dp) where
    pdf (Poisson dist) dp = S.probability (poisson lambda) $ fromIntegral dp
        where
            lambda = (fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)

instance 
    ( PDF (Poisson prob dp)
--     , PlottableDataPoint dp
    , Show prob
    , Show dp
    , Ord dp
    , Ord prob
    , Fractional prob
    , Integral dp
    ) => PlottableDistribution (Poisson prob dp) 
-- instance PlottableDistribution (Poisson Int Double) 
        where

    plotType _ = Points

    samplePoints dist = [min..max]
        where
            min = 0
            max = maximum [20,floor $ 3*lambda]
            lambda = (fromIntegral $ m1 $ pmoments dist) / (fromIntegral $ m0 $ pmoments dist)
    
-- instance (Fractional prob) => Mean (Poisson prob dp) where
--     mean (Poisson dist) = (fromIntegral $ m1 $ pmoments dist) / (fromIntegral $ m0 $ pmoments dist)
-- 
-- instance (Fractional prob) => Variance (Poisson prob dp) where
--     variance dist = mean dist
