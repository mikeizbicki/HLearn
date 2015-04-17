-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Normal
    ( Normal (..)
    , trainNormal
    , train1Normal
    )
    where

import SubHask
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments

-------------------------------------------------------------------------------
-- data types

newtype Normal r = Normal (Moments3 r)
--     deriving (Read,Show,Eq_,Semigroup,Cancellative,Monoid,Group,Abelian,NFData)

deriveHierarchy ''Normal [ ''Eq_, ''Group, ''Abelian]

type instance Datapoint (Normal r) = r
-- type instance Scalar (Normal r) = r

mkNormal :: Ring r => r -> r -> Normal r
mkNormal mu sigma = Normal $ Moments3
    { m0 = 1
    , m1 = mu
    , m2 = sigma*sigma + mu*mu
    }

addNoise :: Ring r => (r -> r) -> r -> Normal r
addNoise f dp = mkNormal dp (f dp)

-------------------------------------------------------------------------------
-- training

{-# INLINE trainNormal #-}
trainNormal :: Ring r => [r] -> Normal r
trainNormal dps = Normal $ foldl1 (+) $ fmap train1Moments3 dps

train1Normal :: Ring r => r -> Normal r
train1Normal dp = Normal $ train1Moments3 dp

-- instance (Num r) => HomTrainer (Normal r) where
--     type Datapoint (Normal r) = r
--     train1dp dp = Normal $ train1dp dp
--
-- type instance Scalar (Normal r) = r

---------------------------------------

-- join :: Normal (Normal r) -> Normal r
-- join (Normal moments) = Normal moments

add :: Ring r => r -> Normal r -> Normal r
add x (Normal moments) = Normal $ moments
    { m1 = m1 moments + m0 moments * x
    , m2 = m2 moments + 2*m1 moments*x + x*x*m0 moments
    }

mul :: Ring r => r -> Normal r -> Normal r
mul x (Normal moments) = Normal $ moments
    { m1 = m1 moments *x
    , m2 = m2 moments *x*x
    }

-------------------------------------------------------------------------------
-- distribution

instance Real r => PDF (Normal r) where
    pdf dist dp = (1 / (sqrt $ sigma2 * 2 * pi))*(exp $ (-1)*(dp-mu)*(dp-mu)/(2*sigma2))
        where
            sigma2 = variance dist
            mu = mean dist

instance Real r => CDF (Normal r) where
    cdf dist dp = ( 0.5 * ( 1 + erf ( (dp - mu) / (sqrt $ sigma2 *2) )))
        where
            sigma2 = variance dist
            mu = mean dist

instance Field r => Mean (Normal r) where
    mean (Normal dist) = m1 dist / m0 dist

instance Field r => Variance (Normal r) where
    variance normal@(Normal dist) = m2 dist / m0 dist - (mean normal)*(mean normal)

-- instance
--     ( Real r
--     , Enum r
--     , Show r
--     , Ord r
--     ) => PlottableDistribution (Normal r) where
--
--     plotType _ = Continuous
--
--     samplePoints dist = samplesFromMinMax min max
--         where
--             min = (mean dist)-5*(sqrt $ variance dist)
--             max = (mean dist)+5*(sqrt $ variance dist)
