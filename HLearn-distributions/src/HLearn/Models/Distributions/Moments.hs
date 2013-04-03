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

module HLearn.Models.Distributions.Moments3
    ( Moments3
--     , Normal
    )
    where

import Control.DeepSeq
import GHC.TypeLits

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- Moments3

data Moments3 prob = Moments3 
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    }
    deriving (Read,Show,Eq,Ord)

mean :: (Fractional prob) => Moments3 prob -> prob
mean dist = m1 dist / m0 dist

variance :: (Fractional prob) => Moments3 prob -> prob
variance dist = m2 dist / m0 dist - (mean dist)*(mean dist)

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
    
instance ModelParams (Moments3 prob) where
    type Params (Moments3 prob) = NoParams
    getparams _ = NoParams

instance (Num prob) => HomTrainer (Moments3 prob) where
    type Datapoint (Moments3 prob) = prob
    train1dp' _ dp = Moments3 1 dp (dp*dp)
    

-------------------------------------------------------------------------------
-- Normal

newtype Normal prob = Normal (Moments3 prob)
    deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
instance ModelParams (Normal prob) where
    type Params (Normal prob) = NoParams
    getparams _ = NoParams

instance (Num prob) => HomTrainer (Normal prob) where
    type Datapoint (Normal prob) = prob
    train1dp' params dp = Normal $ train1dp' params dp

instance (Floating prob) => PDF (Normal prob) prob prob where
    pdf (Normal dist) dp = (1 / (sqrt $ sigma2 * 2 * pi))*(exp $ (-1)*(dp-mu)*(dp-mu)/(2*sigma2))
        where
            sigma2 = variance dist
            mu = mean dist
            