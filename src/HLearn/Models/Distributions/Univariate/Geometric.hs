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

newtype Geometric prob dp = Geometric {  moments :: (Moments3 dp) }
    deriving (Read,Show,Eq,Ord,Monoid,Group)
    
instance (Num dp) => HomTrainer (Geometric prob dp) where
    type Datapoint (Geometric prob dp) = dp
    train1dp dp = Geometric $ train1dp dp

instance (Num dp) => Probabilistic (Geometric prob dp) where
    type Probability (Geometric prob dp) = prob

instance (Integral dp, Floating prob) => PDF (Geometric prob dp) where
    pdf dist dp = p*(1-p)^^dp
        where
            p = geo_p dist

instance 
    ( PDF (Geometric prob dp)
    , Show prob
    , Show dp
    , Ord dp
    , Ord prob
    , Fractional prob
    , RealFrac prob
    , Integral dp
    ) => PlottableDistribution (Geometric prob dp) 
        where

    plotType _ = Points

    samplePoints dist = [min..max]
        where
            min = 0
            max = maximum [20,round $ 3*(fromIntegral $ mean dist)]

geo_p :: (Fractional prob, Integral dp) => Geometric prob dp -> prob
geo_p (Geometric dist) = 1/((fromIntegral $ m1 dist)/(fromIntegral $ m0 dist) +1)
    
instance (Integral dp, RealFrac prob) => Mean (Geometric prob dp) where
    mean dist = round $ 1/(geo_p dist)

instance (Integral dp, Fractional prob) => Variance (Geometric prob dp) where
    variance dist = (1-p)/(p*p)
        where
            p = geo_p dist
