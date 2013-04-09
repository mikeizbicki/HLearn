{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | 
module HLearn.Models.Distributions.Multivariate.Multivariate
    where

import Control.DeepSeq
import Control.Monad.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Container
import HLearn.Models.Distributions.Multivariate.Unital
import HLearn.Models.Distributions.Multivariate.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.Multivariate.MultiNormal hiding (ds)
import HLearn.Models.Distributions.Univariate.Moments
    
-------------------------------------------------------------------------------
-- MultiCat
    
newtype MultiCat (sampleL :: [*]) (basedist :: *) prob = MultiCat
    { catL :: MultiCat' sampleL basedist prob
    }

type family MultiCat' (xs :: [*]) (basedist:: *) prob :: *
type instance MultiCat' '[] basedist prob = basedist
type instance MultiCat' (x ': xs) basedist prob = 
    CatContainer x (MultiCat' xs basedist prob) prob

deriving instance (Read             (MultiCat' sampleL basedist prob)) => Read              (MultiCat sampleL basedist prob)
deriving instance (Show             (MultiCat' sampleL basedist prob)) => Show              (MultiCat sampleL basedist prob)
deriving instance (Eq               (MultiCat' sampleL basedist prob)) => Eq                (MultiCat sampleL basedist prob)
deriving instance (Ord              (MultiCat' sampleL basedist prob)) => Ord               (MultiCat sampleL basedist prob)
deriving instance (Semigroup        (MultiCat' sampleL basedist prob)) => Semigroup         (MultiCat sampleL basedist prob)
deriving instance (RegularSemigroup (MultiCat' sampleL basedist prob)) => RegularSemigroup  (MultiCat sampleL basedist prob)
deriving instance (Monoid           (MultiCat' sampleL basedist prob)) => Monoid            (MultiCat sampleL basedist prob)

instance (ModelParams (MultiCat' sampleL basedist prob)) => ModelParams (MultiCat sampleL basedist prob) where
    type Params (MultiCat sampleL basedist prob) = Params (MultiCat' sampleL basedist prob)
    getparams (MultiCat mc) = getparams mc

instance (HomTrainer (MultiCat' sampleL basedist prob)) => HomTrainer (MultiCat sampleL basedist prob) where
    type Datapoint (MultiCat sampleL basedist prob) = Datapoint (MultiCat' sampleL basedist prob)
    train1dp' params dp = MultiCat $ train1dp' params dp

-------------------------------------------------------------------------------
-- MultiContinuous
    
newtype MultiContinuous (dist :: * -> *) (sampleL :: [*]) (basedist :: *) prob = MultiContinuous
    { multiL :: MultiContinuous' dist sampleL basedist prob
    }

type family MultiContinuous' (dist :: * -> *) (sampleL :: [*]) (basedist :: *) prob
type instance MultiContinuous' dist '[] basedist prob = basedist
type instance MultiContinuous' dist (x ': xs) basedist prob =
    Container dist x (MultiContinuous' dist xs basedist prob) prob

type family Multi' (dist :: * -> * -> * -> *) (sampleL :: [*]) (basedist :: *) prob
type instance Multi' dist '[] basedist prob = basedist
type instance Multi' dist (x ': xs) basedist prob =
    dist x (Multi' dist xs basedist prob) prob

deriving instance (Read             (MultiContinuous' dist sampleL basedist prob)) => Read              (MultiContinuous dist sampleL basedist prob)
deriving instance (Show             (MultiContinuous' dist sampleL basedist prob)) => Show              (MultiContinuous dist sampleL basedist prob)
deriving instance (Eq               (MultiContinuous' dist sampleL basedist prob)) => Eq                (MultiContinuous dist sampleL basedist prob)
deriving instance (Ord              (MultiContinuous' dist sampleL basedist prob)) => Ord               (MultiContinuous dist sampleL basedist prob)
deriving instance (Semigroup        (MultiContinuous' dist sampleL basedist prob)) => Semigroup         (MultiContinuous dist sampleL basedist prob)
deriving instance (RegularSemigroup (MultiContinuous' dist sampleL basedist prob)) => RegularSemigroup  (MultiContinuous dist sampleL basedist prob)
deriving instance (Monoid           (MultiContinuous' dist sampleL basedist prob)) => Monoid            (MultiContinuous dist sampleL basedist prob)

instance (ModelParams (MultiContinuous' dist sampleL basedist prob)) => ModelParams (MultiContinuous dist sampleL basedist prob) where
    type Params (MultiContinuous dist sampleL basedist prob) = Params (MultiContinuous' dist sampleL basedist prob)
    getparams (MultiContinuous mc) = getparams mc

instance (HomTrainer (MultiContinuous' dist sampleL basedist prob)) => HomTrainer (MultiContinuous dist sampleL basedist prob) where
    type Datapoint (MultiContinuous dist sampleL basedist prob) = Datapoint (MultiContinuous' dist sampleL basedist prob)
    train1dp' params dp = MultiContinuous $ train1dp' params dp

-------------------------------------------------------------------------------
-- MultiVariate

-- newtype Multivariate (xs::[* -> * -> *]) prob = Multivariate
--     { multidist :: MultivariateTF xs prob
--     }

-- type family Multivariate (xs :: [[* -> * -> *]]) prob
-- type instance Multivariate xs prob = MultivariateTF (Concat xs) prob

-- deriving instance (Read             (MultivariateTF xs prob)) => Read              (Multivariate xs prob)
-- deriving instance (Show             (MultivariateTF xs prob)) => Show              (Multivariate xs prob)
-- deriving instance (Eq               (MultivariateTF xs prob)) => Eq                (Multivariate xs prob)
-- deriving instance (Ord              (MultivariateTF xs prob)) => Ord               (Multivariate xs prob)
-- deriving instance (Semigroup        (MultivariateTF xs prob)) => Semigroup         (Multivariate xs prob)
-- deriving instance (RegularSemigroup (MultivariateTF xs prob)) => RegularSemigroup  (Multivariate xs prob)
-- deriving instance (Monoid           (MultivariateTF xs prob)) => Monoid            (Multivariate xs prob)
-- 
-- instance (ModelParams (MultivariateTF xs prob)) => ModelParams (Multivariate xs prob) where
--     type Params (Multivariate xs prob) = Params (MultivariateTF xs prob)
--     getparams (Multivariate mc) = getparams mc
-- 
-- instance (HomTrainer (MultivariateTF xs prob)) => HomTrainer (Multivariate xs prob) where
--     type Datapoint (Multivariate xs prob) = Datapoint (MultivariateTF xs prob)
--     train1dp' params dp = Multivariate $ train1dp' params dp

-------------------------------------------------------------------------------
-- test

ds2=[ "test":::'g':::1:::1:::HNil
    , "test":::'f':::1:::2:::HNil
    , "toot":::'f':::2:::2:::HNil
    ]
    
ds= [ "test":::'g':::1:::HNil
    , "test":::'f':::1:::HNil
    , "toot":::'f':::2:::HNil
    ]

testMultivariate = train ds2 :: Multivariate 
    '[ MultiCategorical '[String,Char]
     , Independent2 Normal '[Double,Double]
     ]
     Double

type Multivariate (xs::[[* -> * -> *]]) prob = MultivariateTF (Concat xs) prob

type family MultivariateTF (xs::[* -> * -> *]) prob
type instance MultivariateTF '[] prob = Unital prob
type instance MultivariateTF ((Container univariate prob) ': xs) prob = 
    Container univariate prob (MultivariateTF xs prob) prob
type instance MultivariateTF ((CatContainer' label) ': xs) prob = 
    CatContainer label (MultivariateTF xs prob) prob

type family MultiCategorical (xs :: [*]) :: [* -> * -> *]
type instance MultiCategorical '[] = ('[])
type instance MultiCategorical (x ': xs) = (CatContainer' x) ': (MultiCategorical xs)

type family IIDContinuous (dist :: * -> *) (sampleL :: [*]) :: [* -> * -> *]
type instance IIDContinuous dist '[] = '[]
type instance IIDContinuous dist (x ': xs) = (Container dist x) ': (IIDContinuous dist xs)
