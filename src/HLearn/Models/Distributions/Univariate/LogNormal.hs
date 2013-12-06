-- | LogNormal

module HLearn.Models.Distributions.Univariate.LogNormal
    ( LogNormal
    )
    where

import Debug.Trace
import Data.Number.Erf

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- data types

newtype LogNormal prob dp = LogNormal (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group)
    
-------------------------------------------------------------------------------
-- training

instance (Floating prob) => HomTrainer (LogNormal prob prob) where
    type Datapoint (LogNormal prob prob) = prob
    train1dp dp = LogNormal $ train1dp $ dp

-------------------------------------------------------------------------------
-- distribution

instance Probabilistic (LogNormal prob dp) where
    type Probability (LogNormal prob dp) = prob

instance (Floating prob) => PDF (LogNormal prob prob) where
    pdf (LogNormal dist) dp = (1 / (dp * (sqrt $ s2 * 2 * pi)))*(exp $ (-1)*((log dp)-m)^2/(2*s2))
        where
--             sigma2 = variance dist
--             mu = mean dist
            
            m  = 2*log1 - (1/2)*log1
            s2 = log2 - 2*log1
            
            log1 = log raw1
            log2 = log raw2
            
            raw1 = (m1 dist)/(m0 dist)
            raw2 = (m2 dist)/(m0 dist)

instance (Floating prob, Erf prob) => CDF (LogNormal prob prob) where
    cdf (LogNormal dist) dp = ( 0.5 + 0.5 * ( 1 + erf ( (log (dp - m) ) / (sqrt $ s2 *2) )))
        where
            m = 2*log1 - (1/2) *log1
            s2 = log2 - 2*log1

            log1 = log raw1
            log2 = log raw2
            
            raw1 = (m1 dist)/(m0 dist)
            raw2 = (m2 dist)/(m0 dist)

instance (Floating prob) => Mean (LogNormal prob prob) where
    mean (LogNormal dist) = exp $ m+s2/2
        where
            m  = 2*log1 - (1/2)*log1
            s2 = log2 - 2*log1
            
            log1 = log raw1
            log2 = log raw2
            
            raw1 = (m1 dist)/(m0 dist)
            raw2 = (m2 dist)/(m0 dist)

instance (Show prob, Floating prob) => Variance (LogNormal prob prob) where
    variance (LogNormal dist) = trace ("m="++show m++"; s2="++show s2) $ ((exp s2) -1)*(exp $ 2*m+s2)
        where
            m  = 2*log1 - (1/2)*log1
            s2 = log2 - 2*log1
            
            log1 = log raw1
            log2 = log raw2
            
            raw1 = (m1 dist)/(m0 dist)
            raw2 = (m2 dist)/(m0 dist)

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    ) => PlottableDistribution (LogNormal prob prob) where
    
    plotType _ = Continuous

    samplePoints dist = samplesFromMinMax min max
        where
            min = (mean dist)-5*(sqrt $ variance dist)
            max = (mean dist)+5*(sqrt $ variance dist)
