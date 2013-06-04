-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Geometric
    ( Geometric
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import qualified Statistics.Distribution as S
import Statistics.Distribution.Geometric

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Geometric

newtype Geometric sample prob = Geometric {  moments :: (Moments3 sample) }
    deriving (Read,Show,Eq,Ord,Monoid,Group)
    
instance (Num sample) => HomTrainer (Geometric sample prob) where
    type Datapoint (Geometric sample prob) = sample
    train1dp dp = Geometric $ train1dp dp

instance (Num sample) => Probabilistic (Geometric sample prob) where
    type Probability (Geometric sample prob) = prob

instance (Integral sample, Floating prob) => PDF (Geometric sample prob) where
    pdf dist dp = p*(1-p)^^dp
        where
            p = geo_p dist

instance 
    ( PDF (Geometric sample prob)
    , Show prob
    , Show sample
    , Ord sample
    , Ord prob
    , Fractional prob
    , RealFrac prob
    , Integral sample
    ) => PlottableDistribution (Geometric sample prob) 
        where

    plotType _ = Points

    samplePoints dist = [min..max]
        where
            min = 0
            max = maximum [20,round $ 3*(fromIntegral $ mean dist)]

geo_p :: (Fractional prob, Integral sample) => Geometric sample prob -> prob
geo_p (Geometric dist) = 1/((fromIntegral $ m1 dist)/(fromIntegral $ m0 dist) +1)
    
instance (Integral sample, RealFrac prob) => Mean (Geometric sample prob) where
    mean dist = round $ 1/(geo_p dist)

instance (Integral sample, Fractional prob) => Variance (Geometric sample prob) where
    variance dist = (1-p)/(p*p)
        where
            p = geo_p dist
