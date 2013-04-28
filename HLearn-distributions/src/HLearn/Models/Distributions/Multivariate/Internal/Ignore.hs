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

-- | Used for ignoring data
module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    ( Ignore 
    , Ignore' (Ignore')
    ) where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

newtype Ignore' (label:: *) (basedist:: *) (prob :: *) = Ignore' { basedist :: basedist }
    deriving (Show,Read,Eq,Ord)

type family Ignore (xs :: [*]) :: [* -> * -> *]
type instance Ignore '[] = '[]
type instance Ignore (x ': xs) = (Ignore' x) ': (Ignore xs) 

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian basedist) => Abelian (Ignore' label basedist prob)
instance (Monoid basedist) => Monoid (Ignore' label basedist prob) where
    mempty = Ignore' mempty
    mappend d1 d2 = Ignore' $ mappend (basedist d1) (basedist d2)

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) => HomTrainer (Ignore' label basedist prob) 
        where
    type Datapoint (Ignore' label basedist prob) = label `HCons` (Datapoint basedist)
    
    train1dp (dp:::basedp) = Ignore' $ train1dp basedp

instance (NumDP basedist ring) => NumDP (Ignore' label basedist prob) ring where
    numdp (Ignore' basedist) = numdp basedist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (Ignore' label basedist prob) where
    type Probability (Ignore' label basedist prob) = prob

instance 
    ( Probability basedist ~ prob
    , HomTrainer (Ignore' label basedist prob)
    , Datapoint (Ignore' label basedist prob) ~ HList dpL
    , Datapoint basedist ~ HList basedpL
    , PDF basedist
    ) => PDF (Ignore' label basedist prob)
        where

    {-# INLINE pdf #-}
    pdf dist (label:::basedp) = pdf (basedist dist) basedp

-- instance Marginalize (Nat1Box Zero) (Ignore' label basedist prob) (Unital prob) where
--     getMargin _ dist = Categorical $ Map.map numdp (pdfmap dist) 
    
instance 
    ( Marginalize (Nat1Box n) basedist
    ) => Marginalize (Nat1Box (Succ n)) (Ignore' label basedist prob) 
        where
    type Margin (Nat1Box (Succ n)) (Ignore' label basedist prob) = Margin (Nat1Box n) basedist
    getMargin _ dist = getMargin (undefined :: Nat1Box n) $ basedist dist
    
    type MarginalizeOut (Nat1Box (Succ n)) (Ignore' label basedist prob) = 
        Ignore' label (MarginalizeOut (Nat1Box n) basedist) prob
    marginalizeOut _ dist = Ignore' $ marginalizeOut (undefined :: Nat1Box n) $ basedist dist
    
    condition _ dist dp = Ignore' $ condition (undefined :: Nat1Box n) (basedist dist) dp