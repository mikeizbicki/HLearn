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

newtype Normal prob = Normal (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian,Module,NumDP,NFData)

-------------------------------------------------------------------------------
-- training

instance (Num prob) => HomTrainer (Normal prob) where
    type Datapoint (Normal prob) = prob
    train1dp dp = Normal $ train1dp dp

instance (Num prob) => HasRing (Normal prob) where
    type Ring (Normal prob) = prob

-------------------------------------------------------------------------------
-- algebra

instance (Num prob) => Probabilistic (Normal prob) where
    type Probability (Normal prob) = prob

instance (Floating prob) => PDF (Normal prob) where
    pdf dist dp = (1 / (sqrt $ sigma2 * 2 * pi))*(exp $ (-1)*(dp-mu)*(dp-mu)/(2*sigma2))
        where
            sigma2 = variance dist
            mu = mean dist

instance (Floating prob, Erf prob) => CDF (Normal prob) where
    cdf dist dp = ( 0.5 * ( 1 + erf ( (dp - mu) / (sqrt $ sigma2 *2) )))
        where
            sigma2 = variance dist
            mu = mean dist

instance (Fractional prob) => Mean (Normal prob) where
    mean (Normal dist) = m1 dist / m0 dist

instance (Fractional prob) => Variance (Normal prob) where
    variance normal@(Normal dist) = m2 dist / m0 dist - (mean normal)*(mean normal)

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    ) => PlottableDistribution (Normal prob) where
    
    plotType _ = Continuous

    samplePoints dist = samplesFromMinMax min max
-- fmap (\x -> min+x/(numsamples*(max-min))) [0..numsamples]
        where
            numsamples = 1000
            min = (mean dist)-5*(sqrt $ variance dist)
            max = (mean dist)+5*(sqrt $ variance dist)
