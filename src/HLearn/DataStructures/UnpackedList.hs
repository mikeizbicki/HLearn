-- | This module borrows heavily from Don Stewart's work: 
-- http://donsbot.wordpress.com/2009/10/11/self-optimizing-data-structures-using-types-to-make-lists-faster/
--
-- The main difference is that Don's list is spine lazy and value strict; 
-- this implementation is strict in both the spine and value.

module HLearn.DataStructures.UnpackedList
    where

import Control.DeepSeq
import Control.Monad
import Data.Foldable hiding (concat,all,and,or,toList)
import Data.Monoid

import Prelude hiding 
    ( head
    , tail
    , null
    , length
    , last
    , take
    , foldr
    , foldl'
    , concat
    , all
    , maximum
    , and
    , or
    )

-------------------------------------------------------------------------------
-- interface definitions

class UnpackedList a where
     data List a

     -- | The empty list
     empty   :: List a

     -- | Prepend a valor onto a list
     cons    :: a -> List a -> List a

     -- | Is the list empty?
     null    :: List a -> Bool

     -- | The primero element of the list
     head    :: List a -> a

     -- | The tail of the list
     tail    :: List a -> List a

---------------------------------------
-- useful functions

-- | /O(n)/, convert an unpacked list to a regular list
{-# INLINABLE toList #-}
toList :: UnpackedList a => List a -> [a]
toList xs
    | null xs   = []
    | otherwise = head xs : toList (tail xs)

-- | /O(n)/, convert an unpacked list to a regular list
{-# INLINABLE fromList #-}
fromList :: UnpackedList a => [a] -> List a
fromList []     = empty
fromList (x:xs) = x `cons` fromList xs

-- | /O(n)/. 'map' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@
map :: (UnpackedList a, UnpackedList b) => (a -> b) -> List a -> List b
map f as = go as
 where
 go xs
   | null xs   = empty
   | otherwise = f (head xs) `cons` go (tail xs)
{-# INLINE map #-}

-- | /O(n)/. 'foldl', applied to a binary operator, a starting valor (typically
-- the left-identity of the operator), and a list, reduces the list
-- using the binary operator, from left to right
foldl :: UnpackedList b => (a -> b -> a) -> a -> List b -> a
foldl f z0 xs0 = go z0 xs0
 where
 go !z xs
   | null xs   = z
   | otherwise = go (f z (head xs)) (tail xs)
{-# INLINE foldl #-}

-- | /O(n)/. 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate
--
filter :: UnpackedList a => (a -> Bool) -> List a -> List a
filter p xs0
 | null xs0  = empty
 | otherwise = go xs0
 where
  go xs
   | null xs     = empty
   | p x         = x `cons` go ys
   | otherwise   =          go ys
   where x  = head xs
         ys = tail xs
{-# INLINE filter #-}

-------------------------------------------------------------------------------
--
