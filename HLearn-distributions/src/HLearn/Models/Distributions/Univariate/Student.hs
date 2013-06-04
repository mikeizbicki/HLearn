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
-- data type

newtype Student prob = Student (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian,Module,NumDP,NFData)

-------------------------------------------------------------------------------
-- training

instance (Num prob) => HomTrainer (Student prob) where
    type Datapoint (Student prob) = prob
    train1dp dp = Student $ train1dp dp

instance (Num prob) => HasRing (Student prob) where
    type Ring (Student prob) = prob

-------------------------------------------------------------------------------
-- algebra

instance (Num prob) => Probabilistic (Student prob) where
    type Probability (Student prob) = prob

instance (Gamma prob) => PDF (Student prob) where
    pdf dist dp = ( gamma ( (df + 1) / 2)  /  gamma ( df/2) ) * (1/(sqrt (df * pi) )) * ( 1 + (dp*dp / df) )**( (-1) * ((df + 1) /2)) 
        where
            df = 5 
{-- Degrees of freedom set to 5  until method of moments works --}
    -- df = 6/m3 + 4
{--
instance (Fractional prob) => Mean (Student prob) where
    mean (Student dist) = m1 dist / m0 dist

instance (Fractional prob) => Variance (Student prob) where
    variance student@(Student dist) = m2 dist / m0 dist - (mean student)*(mean student)

instance (Fractional prob) => Kurtosis (Student prob) where
    kurtosis student@(Student dist) = m2 dist / m0 dist - (mean student)*(mean student)
--}

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    , Gamma prob
    ) => PlottableDistribution (Student prob) where
    
    plotType _ = Continuous




