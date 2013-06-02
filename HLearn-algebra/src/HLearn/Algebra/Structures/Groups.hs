-- | These algebraic structures have sacrificed generality in favor of being easily used with the standard Haskell Prelude.  The fact that monoids are not guaranteed to be semigroups makes this difficult.

module HLearn.Algebra.Structures.Groups
    ( 
    -- * Algebra
    Group(..)
    , Abelian (..)

    -- * Non-algebraic
    , FreeInverse(..)
    , Invertible(..)
    
    , module Data.Monoid
    )
    where

import Control.DeepSeq
import Data.Monoid
import GHC.Exts (Constraint)

-------------------------------------------------------------------------------

-- class (c a) => Abelian (a :: *) (c :: * -> Constraint) where
--     op :: a -> a -> a

class (Monoid m) => Abelian m

-------------------------------------------------------------------------------
-- Inverses

-- | Groups are monoids that also have an inverse.  See <https://en.wikipedia.org/wiki/Regular_semigroup>
class (Monoid g) => Group g where
    inverse :: g -> g

-------------------------------------------------------------------------------
-- Invertible

data FreeInverse a = FreeInverse !a 
                   | Negate !a
    deriving (Read,Show,Eq)

instance (Ord a) => Ord (FreeInverse a) where
    compare (FreeInverse x) (FreeInverse y) = compare x y
    compare (Negate x) (Negate y) = compare x y
    compare (FreeInverse x) (Negate y) = case compare x y of
                                      LT -> LT
                                      GT -> GT
                                      EQ -> LT
    compare (Negate x) (FreeInverse y) = case compare x y of
                                      LT -> LT
                                      GT -> GT
                                      EQ -> GT

class Invertible a where
    mkinverse :: a -> a
    isInverse :: a -> a -> Bool
    
instance (Eq a) => Invertible (FreeInverse a) where
    mkinverse (FreeInverse x) = Negate x
    mkinverse (Negate x) = FreeInverse x
    
    isInverse (FreeInverse x) (FreeInverse y) = False
    isInverse (Negate x) (Negate y) = False
    isInverse (FreeInverse x) (Negate y) = x==y
    isInverse (Negate x) (FreeInverse y) = x==y