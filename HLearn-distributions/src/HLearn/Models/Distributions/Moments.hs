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

-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Moments
    ( MomentsParams (..)
    , Moments
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
-- Moments

data MomentsParams prob = MomentsParams
    deriving (Read,Show,Eq,Ord)

data Moments prob = Moments 
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    }
    deriving (Read,Show,Eq,Ord)
    
instance (NFData prob) => NFData (Moments prob) where
    rnf m = deepseq (m0 m) 
          $ deepseq (m1 m) 
          $ deepseq (m2 m)
          $ ()

derivingUnbox "Moments"
    [t| (U.Unbox a) => (Moments a) -> (a, a, a) |]
    [| \ (Moments m0 m1 m2) -> (m0,m1,m2) |]
    [| \ (m0,m1,m2) -> (Moments m0 m1 m2) |]

-------------------------------------------------------------------------------
-- Algebra


instance (Num prob) => Abelian (Moments prob)
instance (Num prob) => Semigroup (Moments prob) where
    (<>) !ma !mb = Moments 
        { m0 = m0 ma + m0 mb
        , m1 = m1 ma + m1 mb
        , m2 = m2 ma + m2 mb
        }
    
instance (Num prob) => Monoid (Moments prob) where
    mappend = (<>)
    mempty = Moments 0 0 0 
    
instance (Num prob) => RegularSemigroup (Moments prob ) where
    inverse !m = Moments (negate $ m0 m) (negate $ m1 m) (negate $ m2 m)

-- instance (Fractional prob, VU.Unbox prob, SingI n) => LeftModule prob (Moments prob n)
-- instance (Fractional prob, VU.Unbox prob) => LeftOperator prob (Moments prob n) where
--     (.*) !p !(Moments vec) = Moments $ VU.map (*p) vec
-- 
-- instance (Fractional prob, VU.Unbox prob, SingI n) => RightModule prob (Moments prob n)
-- instance (Fractional prob, VU.Unbox prob) => RightOperator prob (Moments prob n) where
--     (*.) = flip (.*)
--     
-------------------------------------------------------------------------------
-- Training
    
instance ModelParams (MomentsParams prob) (Moments prob) where
    getparams _ = MomentsParams

instance DefaultParams (MomentsParams prob) (Moments prob) where
    defparams = MomentsParams

instance (Num prob) => HomTrainer (MomentsParams prob) prob (Moments prob) where
    train1dp' _ dp = Moments 1 dp (dp*dp)