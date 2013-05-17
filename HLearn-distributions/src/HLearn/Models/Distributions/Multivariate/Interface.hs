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
    Multivariate
    
    -- * Type functions
--     , Ignore
    , MultiCategorical (..)
    , Independent (..)
    , Dependent (..)
    
    -- * Modules
    , Index (..)
    , module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    , module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    )
    where

import Control.DeepSeq
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.Multivariate.Internal.Container
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.TypeLens
import HLearn.Models.Distributions.Multivariate.MultiNormal

-------------------------------------------------------------------------------
-- Multivariate

-- | this is the main type for specifying multivariate distributions
newtype Multivariate (dp:: *) (xs :: [[* -> * -> *]]) prob = Multivariate (MultivariateTF (Concat xs) prob)

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

deriving instance (Read   (MultivariateTF (Concat xs) prob)) => Read   (Multivariate dp xs prob)
deriving instance (Show   (MultivariateTF (Concat xs) prob)) => Show   (Multivariate dp xs prob)
deriving instance (Eq     (MultivariateTF (Concat xs) prob)) => Eq     (Multivariate dp xs prob)
deriving instance (Ord    (MultivariateTF (Concat xs) prob)) => Ord    (Multivariate dp xs prob)
deriving instance (Monoid (MultivariateTF (Concat xs) prob)) => Monoid (Multivariate dp xs prob)
deriving instance (Group  (MultivariateTF (Concat xs) prob)) => Group  (Multivariate dp xs prob)
deriving instance (NFData (MultivariateTF (Concat xs) prob)) => NFData (Multivariate dp xs prob)
    
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

instance 
    ( Marginalize' (Nat1Box n) (MultivariateTF (Concat xs) prob)
    , MarginalizeOut' (Nat1Box n) (MultivariateTF (Concat xs) prob)
        ~ MultivariateTF (Concat (Replace2D n xs (Ignore' (Index (HList2TypeList (GetHList dp)) n)))) prob
    ) => Marginalize' (Nat1Box n) (Multivariate dp xs prob)
        where   
              
    type Margin' (Nat1Box n) (Multivariate dp xs prob) = Margin' (Nat1Box n) (MultivariateTF (Concat xs) prob)
    getMargin' n (Multivariate dist) = getMargin' n dist
    
    type MarginalizeOut' (Nat1Box n) (Multivariate dp xs prob) = 
        Multivariate dp (Replace2D n xs (Ignore' (Index (HList2TypeList (GetHList dp)) n))) prob
    marginalizeOut' n (Multivariate dist) = Multivariate $ marginalizeOut' n dist
    
    condition' n (Multivariate dist) dp = Multivariate $ condition' n dist dp

type family HList2TypeList hlist :: [a]
type instance HList2TypeList (HList xs) = xs

-- type family Index (xs::[a]) (i::Nat1) :: a
-- type instance Index (x ': xs) Zero = x
-- type instance Index (x ': xs) (Succ i) = Index xs i

type family Replace2D (n :: Nat1) (xs :: [ [ a ] ]) (newval :: a) :: [ [ a ] ]
type instance Replace2D Zero ((x ': xs) ': ys) newval = (newval ': xs) ': ys
type instance Replace2D (Succ n) ((x ': xs) ': ys) newval = AppendFront x (Replace2D n (xs ': ys) newval)
type instance Replace2D n ('[] ': ys) newval = '[] ': (Replace2D n ys newval)

type family AppendFront (x :: a) (xs :: [[a]]) :: [[a]]
type instance AppendFront x (xs ': ys) = (x ': xs) ': ys

data Boxer xs = Boxer

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

