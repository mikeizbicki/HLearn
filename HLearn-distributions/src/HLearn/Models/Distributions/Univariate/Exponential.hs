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

module HLearn.Models.Distributions.Univariate.Exponential
    ( Exponential
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Gnuplot
import HLearn.Models.Distributions.Univariate.Internal.Moments

-------------------------------------------------------------------------------
-- Exponential

newtype Exponential prob = Exponential (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
instance (Num prob) => HomTrainer (Exponential prob) where
    type Datapoint (Exponential prob) = prob
    train1dp dp = Exponential $ train1dp dp

instance (Num prob) => Distribution (Exponential prob) where
    type Probability (Exponential prob) = prob

instance (Floating prob) => PDF (Exponential prob) where
    pdf dist dp = lambda*(exp $ (-1)*lambda*dp)
        where
            lambda = e_lambda dist

e_lambda (Exponential dist) = (m0 dist)/(m1 dist)

instance (Fractional prob) => Mean (Exponential prob) where
    mean dist = 1/(e_lambda dist)

instance (Fractional prob) => Variance (Exponential prob) where
    variance dist = 1/(e_lambda dist)^^2

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    ) => PlottableDistribution (Exponential prob) where
    
    plotType _ = Continuous

    samplePoints dist = samplesFromMinMax 0 $ (mean dist)*3
