{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Algebraic instances for the HVector type.  These form the data points used for much of the library.
module HLearn.Algebra.HVector
    ( module Data.Vector.Heterogenous
    , L2
    )
    where

import Data.Vector.Heterogenous

import HLearn.Algebra.HomTrainer
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.MetricSpace
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Triangles

-------------------------------------------------------------------------------
-- Algebra

instance Abelian (HList '[])
instance (Abelian x, Abelian (HList xs)) => Abelian (HList (x ': xs))
instance (Abelian (HList xs), ValidHVector box xs) => Abelian (HVector box xs)

---------------------------------------

instance Group (HList '[]) where
    inverse HNil = HNil

instance (Group x, Group (HList xs)) => Group (HList (x ': xs)) where
    inverse (x:::xs) = inverse x:::inverse xs

instance (Group (HList xs), ValidHVector box xs) => Group (HVector box xs) where
    inverse hv = vec (undefined::a->box) $ inverse $ toHList hv

---------------------------------------

instance (HasRing x) => HasRing (HList (x ': xs)) where
    type Ring (HList (x ': xs)) = Ring x
    
instance (Module x, Module (HList xs), Ring (HList xs) ~ Ring x) => Module (HList (x ': xs)) where
    r .* (x:::xs) = (r.*x):::(r.*xs)
    
instance (HasRing x) => HasRing (HList (x ': '[])) where
    type Ring (HList (x ': '[])) = Ring x
    
instance (Module x) => Module (HList (x ': '[])) where
    r .* (x:::HNil) = (r.*x):::HNil

---------------------------------------

newtype L2 xs = L2 xs

instance (Num r) => Abelian (L2 [r])
instance (Num r) => Monoid (L2 [r]) where
    mempty = L2 $ repeat 0
    mappend (L2 xs) (L2 ys) = L2 $ zipWith (+) xs ys
    
instance (Num r) => Group (L2 [r]) where
    inverse (L2 xs) = L2 $ map (*(-1)) xs
   
instance (Num r) => HasRing (L2 [r]) where
    type Ring (L2 [r]) = r

instance (Num r) => Module (L2 [r]) where
--     type Ring (L2 [r]) = r
    r .* (L2 xs) = L2 $ map (r*) xs

instance (Floating r) => MetricSpace (L2 [r]) where
    distance (L2 xs) (L2 ys) = sqrt . sum . map (^2) $ zipWith (-) xs ys

-- instance (Num a) => HasRing a where
--     type Ring a = a
-- 
-- instance Monoid Double where
--     mempty = 0
--     mappend = (+)
--     
-- instance Group Double where
--     inverse = negate
-- 
-- instance Abelian Double
-- 
-- instance Module Double where
--     (.*) = (*)

-- instance (LeftOperator r x, LeftOperator r (HList xs)) => LeftOperator r (HList (x ': xs)) where
--     r .* (x:::xs) = (r.*x):::(r.*xs)
-- 
-- instance (LeftOperator r (HList xs), ValidHVector box xs) => LeftOperator r (HVector box xs) where
--     r .* hv = vec (undefined::a->box) $ r .* toHList hv
-- 
-- instance (Num r) => LeftModule r (HList '[])
-- instance (LeftModule r x, LeftModule r (HList xs)) => LeftModule r (HList (x ': xs))
-- instance (LeftModule r (HList xs), ValidHVector box xs) => LeftModule r (HVector box xs)
-- 
-- instance RightOperator r (HList '[]) where
--     HNil *. r = HNil
--     
-- instance (RightOperator r x, RightOperator r (HList xs)) => RightOperator r (HList (x ': xs)) where
--     (x:::xs) *. r = (x*.r):::(xs*.r)
-- 
-- instance (RightOperator r (HList xs), ValidHVector box xs) => RightOperator r (HVector box xs) where
--     hv*.r = vec (undefined::a->box) $ toHList hv *. r
-- 
-- instance (Num r) => RightModule r (HList '[])
-- instance (RightModule r x, RightModule r (HList xs)) => RightModule r (HList (x ': xs))
-- instance (RightModule r (HList xs), ValidHVector box xs) => RightModule r (HVector box xs)

-------------------------------------------------------------------------------
-- Training

instance HomTrainer (HList '[]) where
    type Datapoint (HList '[]) = (HList '[])
    train1dp HNil = HNil

instance 
    ( HomTrainer model
    , HomTrainer (HList modelL)
    , Datapoint (HList modelL) ~ HList datapointL
    ) => HomTrainer (HList (model ': modelL))
    where
        type Datapoint (HList (model ': modelL)) = (HList ((Datapoint model) ': (UnHList (Datapoint (HList modelL)))))
        train1dp (dp:::dpL) = train1dp dp ::: train1dp dpL
        