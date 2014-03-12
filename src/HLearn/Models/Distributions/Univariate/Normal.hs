-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Normal
    ( Normal (..)
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving
import Math.Gamma
import Data.Number.Erf

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

instance (Num prob) => HomTrainer (Normal prob prob) where
    type Datapoint (Normal prob prob) = prob
    train1dp dp = Normal $ train1dp dp

type instance Scalar (Normal prob dp) = prob

---------------------------------------

join :: Normal prob (Normal prob dp) -> Normal prob dp
join (Normal moments) = Normal moments

add :: (Num prob) => prob -> Normal prob prob -> Normal prob prob
add x (Normal moments) = Normal $ moments
    { m1 = m1 moments + m0 moments * x
    , m2 = m2 moments + 2*m1 moments*x + x*x*m0 moments
    }

mul :: (Num prob) => prob -> Normal prob prob -> Normal prob prob
mul x (Normal moments) = Normal $ moments
    { m1 = m1 moments *x
    , m2 = m2 moments *x*x 
    }

-------------------------------------------------------------------------------
-- distribution

instance (Num prob) => Probabilistic (Normal prob dp) where
    type Probability (Normal prob dp) = prob

instance (Floating prob) => PDF (Normal prob prob) where
    pdf dist dp = (1 / (sqrt $ sigma2 * 2 * pi))*(exp $ (-1)*(dp-mu)*(dp-mu)/(2*sigma2))
        where
            sigma2 = variance dist
            mu = mean dist

instance (Floating prob, Erf prob) => CDF (Normal prob prob) where
    cdf dist dp = ( 0.5 * ( 1 + erf ( (dp - mu) / (sqrt $ sigma2 *2) )))
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
