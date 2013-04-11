{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Algebraic instances for the HVector type.  These form the data points used for much of the library.
module HLearn.Algebra.HVector
    ( module Data.Vector.Heterogenous
    )
    where

import Data.Vector.Heterogenous

import HLearn.Algebra.HomTrainer
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Triangles

-------------------------------------------------------------------------------
-- Algebra

instance Semigroup (HList '[]) where
    HNil <> HNil = HNil

instance (Semigroup x, Semigroup (HList xs)) => Semigroup (HList (x ': xs)) where
    (x:::xs)<>(y:::ys) = (x<>y):::(xs<>ys)

instance (Semigroup (HList xs), ValidHVector box xs) => Semigroup (HVector box xs) where
    v1 <> v2 = vec (undefined::a->box) $ (toHList v1)<>(toHList v2)

instance Abelian (HList '[])
instance (Abelian x, Abelian (HList xs)) => Abelian (HList (x ': xs))
instance (Abelian (HList xs), ValidHVector box xs) => Abelian (HVector box xs)

---------------------------------------

instance RegularSemigroup (HList '[]) where
    inverse HNil = HNil

instance (RegularSemigroup x, RegularSemigroup (HList xs)) => RegularSemigroup (HList (x ': xs)) where
    inverse (x:::xs) = inverse x:::inverse xs

instance (RegularSemigroup (HList xs), ValidHVector box xs) => RegularSemigroup (HVector box xs) where
    inverse hv = vec (undefined::a->box) $ inverse $ toHList hv

---------------------------------------

instance LeftOperator r (HList '[]) where
    r .* HNil = HNil
    
instance (LeftOperator r x, LeftOperator r (HList xs)) => LeftOperator r (HList (x ': xs)) where
    r .* (x:::xs) = (r.*x):::(r.*xs)

instance (LeftOperator r (HList xs), ValidHVector box xs) => LeftOperator r (HVector box xs) where
    r .* hv = vec (undefined::a->box) $ r .* toHList hv

instance (Num r) => LeftModule r (HList '[])
instance (LeftModule r x, LeftModule r (HList xs)) => LeftModule r (HList (x ': xs))
instance (LeftModule r (HList xs), ValidHVector box xs) => LeftModule r (HVector box xs)

instance RightOperator r (HList '[]) where
    HNil *. r = HNil
    
instance (RightOperator r x, RightOperator r (HList xs)) => RightOperator r (HList (x ': xs)) where
    (x:::xs) *. r = (x*.r):::(xs*.r)

instance (RightOperator r (HList xs), ValidHVector box xs) => RightOperator r (HVector box xs) where
    hv*.r = vec (undefined::a->box) $ toHList hv *. r

instance (Num r) => RightModule r (HList '[])
instance (RightModule r x, RightModule r (HList xs)) => RightModule r (HList (x ': xs))
instance (RightModule r (HList xs), ValidHVector box xs) => RightModule r (HVector box xs)

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
        