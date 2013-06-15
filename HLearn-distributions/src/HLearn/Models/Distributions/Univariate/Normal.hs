-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Normal
    ( Normal (..)
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- data types

newtype Normal prob dp = Normal (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian,Module,NumDP,NFData)

mkNormal :: (Num prob) => prob -> prob -> Normal prob dp
mkNormal mu sigma = Normal $ Moments3
    { m0 = 1
    , m1 = mu
    , m2 = sigma*sigma + mu*mu
    }

addNoise :: (Num prob) => (prob -> prob) -> prob -> Normal prob prob
addNoise f dp = mkNormal dp (f dp)

-------------------------------------------------------------------------------
-- training

instance (Num prob) => HomTrainer (Normal prob (Normal prob dp)) where
    type Datapoint (Normal prob (Normal prob dp)) = Normal prob dp
    train1dp (Normal dp) = Normal dp

instance (Num prob) => HomTrainer (Normal prob prob) where
    type Datapoint (Normal prob prob) = prob
    train1dp dp = Normal $ train1dp dp

instance (Num prob) => HasRing (Normal prob dp) where
    type Ring (Normal prob dp) = prob

---------------------------------------

join :: Normal prob (Normal prob dp) -> Normal prob dp
join (Normal moments) = Normal moments

-------------------------------------------------------------------------------
-- distribution

instance (Num prob) => Probabilistic (Normal prob dp) where
    type Probability (Normal prob dp) = prob

instance (Floating prob) => PDF (Normal prob prob) where
    pdf dist dp = (1 / (sqrt $ sigma2 * 2 * pi))*(exp $ (-1)*(dp-mu)*(dp-mu)/(2*sigma2))
        where
            sigma2 = variance dist
            mu = mean dist

instance (Fractional prob) => Mean (Normal prob prob) where
    mean (Normal dist) = m1 dist / m0 dist

instance (Fractional prob) => Variance (Normal prob prob) where
    variance normal@(Normal dist) = m2 dist / m0 dist - (mean normal)*(mean normal)

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    ) => PlottableDistribution (Normal prob prob) where
    
    plotType _ = Continuous

    samplePoints dist = samplesFromMinMax min max
        where
            min = (mean dist)-5*(sqrt $ variance dist)
            max = (mean dist)+5*(sqrt $ variance dist)

-------------------------------------------------------------------------------
-- test

dp1 = mkNormal 1 1
dp2 = mkNormal 10 1

model = train [dp1,dp2] :: Normal Double (Normal Double Double)