{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}

-- | These algebraic structures have sacrificed generality in favor of being easily used with the standard Haskell Prelude.  The fact that monoids are not guaranteed to be semigroups makes this difficult.

module HLearn.Algebra.Structures
    ( 
    -- * Type classes
    RegularSemigroup (..)
    , Group(..)
    , Abelian (..)
    
    -- * Free Structures
    , RegSG2Group (..)

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

-------------------------------------------------------------------------------
-- RegSG2Group

-- | Convert any regular semigroup into a group (and thus also a monoid) by adding a unique identity element
data (RegularSemigroup sg) => RegSG2Group sg = SGNothing | SGJust sg
    deriving (Show,Read,Ord,Eq)

instance (RegularSemigroup sg) => Semigroup (RegSG2Group sg) where
    SGNothing <> m = m
    m <> SGNothing = m
    (SGJust sg1) <> (SGJust sg2) = SGJust $ sg1<>sg2

instance (RegularSemigroup sg) => RegularSemigroup (RegSG2Group sg) where
    inverse SGNothing = SGNothing
    inverse (SGJust x) = SGJust $ inverse x

instance (RegularSemigroup sg) => Monoid (RegSG2Group sg) where
    mempty = SGNothing
    mappend = (<>)
    
instance (RegularSemigroup sg) => Group (RegSG2Group sg)

instance (RegularSemigroup sg, NFData sg) => NFData (RegSG2Group sg) where
    rnf SGNothing = ()
    rnf (SGJust sg) = rnf sg