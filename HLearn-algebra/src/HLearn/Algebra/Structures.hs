{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Structures
    ( Invertible (..)
    , InverseSemigroup (..)
    , HasIdentity (..)
    , SG2Monoid (..)
    , Group(..)
    , module Data.Semigroup
    )
    where

import Data.Semigroup

-------------------------------------------------------------------------------
-- Free Group

instance Invertible Bool where
    inverse b 
        | b==True   = False
        | otherwise = True

newtype Invert datatype = Invert (Bool,datatype)

instance Invertible (Invert datatype) where
    inverse (Invert (bool,datatype)) = Invert (inverse bool, datatype)

data FreeGroup datatype = FreeGroup [datatype]



-------------------------------------------------------------------------------
-- Inverse Semigroups

class Invertible i where
    inverse :: i -> i

class (Invertible isg, Semigroup isg) => InverseSemigroup isg

instance (Invertible isg, Semigroup isg) => InverseSemigroup isg

-------------------------------------------------------------------------------
-- Monoids

class HasIdentity i where
    identity :: i

-- instance (HasIdentity m, Semigroup m) => Monoid m where
--     mempty = identity
--     mappend = (<>)

data (Semigroup sg) => SG2Monoid sg = SGNothing | SGJust sg

instance (Semigroup sg) => Monoid (SG2Monoid sg) where
    mempty = SGNothing
    mappend SGNothing m = m
    mappend m SGNothing = m
    mappend (SGJust sg1) (SGJust sg2) = SGJust $ sg1<>sg2

instance (Semigroup sg) => Semigroup (SG2Monoid sg) where
    (<>) = mappend

-------------------------------------------------------------------------------
-- Groups

class (InverseSemigroup g, HasIdentity g) => Group g

instance (InverseSemigroup g, HasIdentity g) => Group g
