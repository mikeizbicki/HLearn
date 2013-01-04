{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}

-- | These algebraic structures have sacrificed generality in favor of being easily used with the standard Haskell Prelude.  The fact that monoids are not guaranteed to be semigroups makes this difficult.

module HLearn.Algebra.Structures.Groups
    ( 
    -- * Type classes
    RegularSemigroup (..)
    , Group(..)
    , Abelian (..)

    , module Data.Semigroup
    )
    where

import Control.DeepSeq
import Data.Semigroup
import GHC.Exts (Constraint)

-------------------------------------------------------------------------------

-- class (c a) => Abelian (a :: *) (c :: * -> Constraint) where
--     op :: a -> a -> a

class (Semigroup sg) => Abelian sg

-------------------------------------------------------------------------------
-- Inverses

-- | Semigroups that also have an inverse.  See <https://en.wikipedia.org/wiki/Regular_semigroup>
class (Semigroup g) => RegularSemigroup g where
    inverse :: g -> g

-- -- | Semigroups where a unique inverse exists for each element.  See <https://en.wikipedia.org/wiki/Inverse_semigroup>
-- class (RegularSemigroup g) => InverseSemigroup g

-- | Regular semigroups that also have an identity; alternatively, monoids where every element has a unique inverse.  See <https://en.wikipedia.org/wiki/Group_(mathematics)>
class (RegularSemigroup g, Monoid g) => Group g
instance (RegularSemigroup g, Monoid g) => Group g
