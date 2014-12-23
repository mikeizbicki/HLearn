{-# LANGUAGE TemplateHaskell #-}

-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Internal.Moments
    ( Moments3(..)
    , train1Moments3
    )
    where

import SubHask
import SubHask.Monad
import HLearn.Models.Distributions.Common
-- import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Moments3

data Moments3 prob = Moments3
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    }
    deriving (Read,Show)

type instance Logic (Moments3 prob) = Logic prob

instance Eq prob => Eq_ (Moments3 prob) where
    x==y = m0 x==m0 y && m1 x==m1 y && m2 x==m2 y

instance (NFData prob) => NFData (Moments3 prob) where
    rnf m = seq m ()

-- derivingUnbox "Moments3"
--     [t| (U.Unbox a) => (Moments3 a) -> (a, a, a) |]
--     [| \ (Moments3 m0 m1 m2) -> (m0,m1,m2) |]
--     [| \ (m0,m1,m2) -> (Moments3 m0 m1 m2) |]

-------------------------------------------------------------------------------
-- Algebra

instance Semigroup r => Semigroup (Moments3 r) where
    ma + mb = Moments3
        { m0 = m0 ma + m0 mb
        , m1 = m1 ma + m1 mb
        , m2 = m2 ma + m2 mb
        }

instance Monoid r => Monoid (Moments3 r) where
    zero = Moments3 zero zero zero

instance Abelian r => Abelian (Moments3 r)

instance Cancellative r => Cancellative (Moments3 r) where
    ma - mb = Moments3
        { m0 = m0 ma - m0 mb
        , m1 = m1 ma - m1 mb
        , m2 = m2 ma - m2 mb
        }

instance Group r => Group (Moments3 r) where
    negate !m = Moments3 (negate $ m0 m) (negate $ m1 m) (negate $ m2 m)

type instance Scalar (Moments3 r) = r

-- instance (Fractional prob, VU.Unbox prob, SingI n) => LeftModule prob (Moments3 prob n)
-- instance (Fractional prob, VU.Unbox prob) => LeftOperator prob (Moments3 prob n) where
--     (.*) !p !(Moments3 vec) = Moments3 $ VU.map (*p) vec
--
-- instance (Fractional prob, VU.Unbox prob, SingI n) => RightModule prob (Moments3 prob n)
-- instance (Fractional prob, VU.Unbox prob) => RightOperator prob (Moments3 prob n) where
--     (*.) = flip (.*)
--
-------------------------------------------------------------------------------
-- Training

train1Moments3 :: Ring r => r -> Moments3 r
train1Moments3 r = Moments3 1 r (r*r)

-- instance (Num prob) => HomTrainer (Moments3 prob) where
--     type Datapoint (Moments3 prob) = prob
--     train1dp dp = Moments3 1 dp (dp*dp)
--
-- instance (Num prob) => NumDP (Moments3 prob) where
--     numdp = m0

