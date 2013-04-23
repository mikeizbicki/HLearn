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

-- | Used for Multivariate distributions

module HLearn.Models.Distributions.Multivariate.Interface
    (
    Trainable (..)
    , Multivariate
    
    -- * Type functions
--     , Ignore
    , MultiCategorical (..)
    , Independent (..)
    , Dependent (..)
    
    -- * Modules
    , module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.Multivariate.Internal.Container
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.MultiNormal

-------------------------------------------------------------------------------
-- data types

-- data Ignore (datapoint:: *) (basedist:: *) (prob:: *) = Ignore
--     deriving (Read,Show,Eq,Ord)
--     
-- instance Semigroup (Ignore datapoint basedist prob) where
--     Ignore <> Ignore = Ignore
--     
-- instance Monoid (Ignore datapoint basedist prob) where
--     mempty = Ignore
--     mappend = (<>)
--     
-- instance HomTrainer (Ignore datapoint basedist prob) where
--     type Datapoint (Ignore datapoint basedist prob) = datapoint `HCons` (Datapoint basedist)
--     train1dp _ = Ignore
--     
-- instance Probabilistic (Ignore datapoint basedist prob) where
--     type Probability (Ignore datapoint basedist prob) = prob
-- 
-- instance (Num prob) => PDF (Ignore datapoint basedist prob) where
--     pdf _ _ = 1

-- | The Trainable class allows us to convert data types into an isomorphic "HList"s.  All of our multivariate distributions work on "HList"s, so they work on all instances of "Trainable" as well.
class Trainable t where
    type GetHList t
    getHList :: t -> GetHList t

instance Trainable (HList '[]) where
    type GetHList (HList '[]) = HList '[]
    getHList t = t
    
instance (Trainable (HList xs)) => Trainable (HList (x ': xs)) where
    type GetHList (HList (x ': xs)) = HList (x ': xs)
    getHList t = t

-------------------------------------------------------------------------------
-- Multivariate

newtype Multivariate dp (xs :: [[* -> * -> *]]) prob = Multivariate (MultivariateTF (Concat xs) prob)

type family MultivariateTF (xs::[* -> * -> *]) prob
type instance MultivariateTF '[] prob = Unital prob
type instance MultivariateTF ((Container univariate sample) ': xs) prob = 
    Container univariate sample (MultivariateTF xs prob) prob
type instance MultivariateTF ((MultiContainer dist sample) ': xs) prob = 
    MultiContainer dist sample (MultivariateTF xs prob) prob
type instance MultivariateTF ((CatContainer label) ': xs) prob = 
    CatContainer label (MultivariateTF xs prob) prob
type instance MultivariateTF ((Ignore' label) ': xs) prob = 
    Ignore' label (MultivariateTF xs prob) prob

deriving instance (Read             (MultivariateTF (Concat xs) prob)) => Read              (Multivariate dp xs prob)
deriving instance (Show             (MultivariateTF (Concat xs) prob)) => Show              (Multivariate dp xs prob)
deriving instance (Eq               (MultivariateTF (Concat xs) prob)) => Eq                (Multivariate dp xs prob)
deriving instance (Ord              (MultivariateTF (Concat xs) prob)) => Ord               (Multivariate dp xs prob)
deriving instance (Semigroup        (MultivariateTF (Concat xs) prob)) => Semigroup         (Multivariate dp xs prob)
deriving instance (Monoid           (MultivariateTF (Concat xs) prob)) => Monoid            (Multivariate dp xs prob)
deriving instance (RegularSemigroup (MultivariateTF (Concat xs) prob)) => RegularSemigroup  (Multivariate dp xs prob)
    
instance 
    ( HomTrainer (MultivariateTF (Concat xs) prob)
    , Trainable dp
    , GetHList dp ~ Datapoint (MultivariateTF (Concat xs) prob)
    ) => HomTrainer (Multivariate dp xs prob) 
        where
    type Datapoint (Multivariate dp xs prob) = dp
    train1dp dp = Multivariate $ train1dp $ getHList dp
    
instance Probabilistic (Multivariate dp xs prob) where
    type Probability (Multivariate dp xs prob) = prob
    
instance 
    ( PDF (MultivariateTF (Concat xs) prob)
    , Probability (MultivariateTF (Concat xs) prob) ~ prob
    , Datapoint (MultivariateTF (Concat xs) prob) ~ GetHList dp
    , Trainable dp
    , HomTrainer (Multivariate dp xs prob)
    ) => PDF (Multivariate dp xs prob) 
        where
    pdf (Multivariate dist) dp = pdf dist (getHList dp)    

-------------------------------------------------------------------------------
-- Type functions
    
-- type Multivariate (xs::[[* -> * -> *]]) prob = MultivariateTF (Concat xs) prob

type family MultiCategorical (xs :: [*]) :: [* -> * -> *]
type instance MultiCategorical '[] = ('[])
type instance MultiCategorical (x ': xs) = (CatContainer x) ': (MultiCategorical xs)

-- type Dependent dist (xs :: [*]) = '[ MultiContainer (dist xs) xs ]
type family Dependent (dist::a) (xs :: [*]) :: [* -> * -> *]
type instance Dependent dist xs = '[ MultiContainer (dist xs) xs ]

type family Independent (dist :: a) (sampleL :: [*]) :: [* -> * -> *]
type instance Independent dist '[] = '[]
type instance Independent (dist :: * -> *) (x ': xs) = (Container dist x) ': (Independent dist xs)
type instance Independent (dist :: * -> * -> *)  (x ': xs) = (Container (dist x) x) ': (Independent dist xs)

