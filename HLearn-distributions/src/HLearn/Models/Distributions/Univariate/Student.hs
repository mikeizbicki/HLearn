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

module HLearn.Models.Distributions.Univariate.Student
    ( Student (..)
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving
import Math.Gamma

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Internal.Moments
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- data types
{-- <TODO>
newtype Student prob = Student (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian,Module,NumDP,NFData)
--}
-------------------------------------------------------------------------------
-- training
{-- <TODO>
instance (Num prob) => HomTrainer (Student prob) where
    type Datapoint (Student prob) = prob
    train1dp dp = Student $ train1dp dp

instance (Num prob) => HasRing (Student prob) where
    type Ring (Student prob) = prob
--}
-------------------------------------------------------------------------------
-- algebra

instance (Num prob) => Probabilistic (Student prob) where
    type Probability (Student prob) = prob

{-- there are more efficent ways to do this --}
instance (Floating prob) => PDF (Student prob) where
    pdf dist dp mu sigma df = ( gamma ( (df + 1) / 2)  / sqrt (df * pi) * gamma ( df/2) ) *  ( 1 + (x*x / df) )**( (-1) * ((df + 1) /2)) 
        where
            sigma2 = variance dist
            mu = mean dist

instance (Fractional prob) => Mean (Student prob) where
    mean (Student dist) = m1 dist / m0 dist

instance (Fractional prob) => Variance (Student prob) where
    variance Student@(Student dist) = m2 dist / m0 dist - (mean Student)*(mean Student)

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    ) => PlottableDistribution (Student prob) where
    
    plotType _ = Continuous




