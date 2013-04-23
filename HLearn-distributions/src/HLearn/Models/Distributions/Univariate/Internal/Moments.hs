{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Internal.Moments
    ( Moments3(..)
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
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Moments3

data Moments3 prob = Moments3 
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    }
    deriving (Read,Show,Eq,Ord)

instance (NFData prob) => NFData (Moments3 prob) where
    rnf m = deepseq (m0 m) 
          $ deepseq (m1 m) 
          $ deepseq (m2 m)
          $ ()

derivingUnbox "Moments3"
    [t| (U.Unbox a) => (Moments3 a) -> (a, a, a) |]
    [| \ (Moments3 m0 m1 m2) -> (m0,m1,m2) |]
    [| \ (m0,m1,m2) -> (Moments3 m0 m1 m2) |]

-------------------------------------------------------------------------------
-- Algebra

instance (Num prob) => Abelian (Moments3 prob)
instance (Num prob) => Semigroup (Moments3 prob) where
    (<>) !ma !mb = Moments3 
        { m0 = m0 ma + m0 mb
        , m1 = m1 ma + m1 mb
        , m2 = m2 ma + m2 mb
        }
    
instance (Num prob) => Monoid (Moments3 prob) where
    mappend = (<>)
    mempty = Moments3 0 0 0 
    
instance (Num prob) => RegularSemigroup (Moments3 prob ) where
    inverse !m = Moments3 (negate $ m0 m) (negate $ m1 m) (negate $ m2 m)

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
    
instance (Num prob) => HomTrainer (Moments3 prob) where
    type Datapoint (Moments3 prob) = prob
    train1dp dp = Moments3 1 dp (dp*dp)
    


-------------------------------------------------------------------------------
-- LogNormal

-- newtype LogNormal prob = LogNormal (Moments3 prob)
--     deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
--     
-- instance ModelParams (LogNormal prob) where
--     type Params (LogNormal prob) = NoParams
--     getparams _ = NoParams
-- 
-- instance (Num prob) => HomTrainer (LogNormal prob) where
--     type Datapoint (LogNormal prob) = prob
--     train1dp' params dp = LogNormal $ train1dp' params dp
-- 
-- instance (Num prob) => Distribution (LogNormal prob) where
--     type Probability (LogNormal prob) = prob
-- 
-- instance (Eq prob, Floating prob) => PDF (LogNormal prob) where
--     pdf (LogNormal dist) 0  = 0
--     pdf (LogNormal dist) dp = (1/(s*dp*(sqrt $ 2*pi)))*(exp $ (-1)*((log dp)-m)**2/(2*s*s))
--         where
--             m = 0
--             s = 1


-------------------------------------------------------------------------------
-- Binomial

newtype Binomial sample prob = Binomial {  bmoments :: (Moments3 sample) }
    deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
instance (Num sample) => HomTrainer (Binomial sample prob) where
    type Datapoint (Binomial sample prob) = sample
    train1dp dp = Binomial $ train1dp dp

instance (Num sample) => Probabilistic (Binomial sample prob) where
    type Probability (Binomial sample prob) = prob
    
instance (Floating prob) => PDF (Binomial Int Double) where
    pdf (Binomial dist) dp = S.probability (binomial n p) dp
        where
            n = bin_n $ Binomial dist
            p = bin_p $ Binomial dist

bin_n :: Binomial Int Double -> Int
bin_n (Binomial dist) = round $ ((fromIntegral $ m1 dist :: Double) / (fromIntegral $ m0 dist)) / (bin_p $ Binomial dist)

bin_p :: Binomial Int Double -> Double
bin_p (Binomial dist) = ((fromIntegral $ m1 dist) / (fromIntegral $ m0 dist)) + 1 - (fromIntegral $ m2 dist)/(fromIntegral $ m1 dist)

instance 
    ( PDF (Binomial sample prob)
--     , PlottableDataPoint sample
    , Show prob
    , Show sample
    , Ord sample
    , Ord prob
    , Num prob
    , Integral sample
    ) => PlottableDistribution (Binomial sample prob) 
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
