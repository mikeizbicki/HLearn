{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Structures
    ( Invertible (..)
    , InverseSemigroup (..)
    , HasIdentity(..)
    , Group(..)
    , module Data.Semigroup
    )
    where

import Data.Semigroup

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

-------------------------------------------------------------------------------
-- Groups

class (InverseSemigroup g, HasIdentity g) => Group g

instance (InverseSemigroup g, HasIdentity g) => Group g
