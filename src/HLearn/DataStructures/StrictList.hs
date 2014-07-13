module HLearn.DataStructures.StrictList
    where

import Control.DeepSeq
import Control.Monad
import Data.Foldable hiding (concat,all,and,or)
import Data.Monoid
import Prelude hiding 
    ( length
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
-- data types

data List a = (:.) !a !(List a) | Nil
    deriving (Read,Show,Eq,Ord)

infixr 5 :.

instance NFData a => NFData (List a) where
    rnf Nil = ()
    rnf (x:.Nil) = rnf x
    rnf (x:.xs) = deepseq x $ rnf xs

instance Monoid (List a) where
    {-# INLINE mempty #-}
    mempty = Nil

    {-# INLINE mappend #-}
    mappend Nil ys = ys
    mappend (x:.xs) ys = x:.(mappend xs ys)

instance Functor List where
    {-# INLINE fmap #-}
    fmap f Nil = Nil
    fmap f (x:.xs) = (f x):.(fmap f xs)

instance Foldable List where
    {-# INLINE [0] foldr #-}
    foldr k z = go
        where
            go Nil     = z
            go (y:.ys) = y `k` go ys

    {-# INLINE foldl' #-}
    foldl' !f !a Nil     = a
    foldl' !f !a (x:.xs) = let a' = f a x in a' `seq` foldl' f a' xs

instance Monad List where
    {-# INLINE return #-}
    return a = a:.Nil

    {-# INLINE (>>=) #-}
    xs >>= f = concat $ fmap f xs

-------------------------------------------------------------------------------
-- special functions

{-# INLINE strictlist2list #-}
strictlist2list :: List a -> [a]
strictlist2list Nil = []
strictlist2list (x:.xs) = x:(strictlist2list xs)

{-# INLINE list2strictlist #-}
list2strictlist :: [a] -> List a
list2strictlist [] = Nil
list2strictlist (x:xs) = x:.list2strictlist xs

-------------------------------------------------------------------------------
-- Prelude functions

{-# INLINABLE maximum #-}
maximum (x:.xs) = go x xs
    where
        go m Nil = m
        go m (y:.ys) = go (if m>y then m else y) ys

{-# INLINABLE all #-}
all f Nil = True
all f (x:.xs) = f x && all f xs

{-# INLINABLE and #-}
and Nil = True
and (x:.xs) = x && and xs

{-# INLINABLE or #-}
or Nil = False
or (x:.xs) = x || or xs

{-# INLINABLE concat #-}
concat :: List (List a) -> List a
concat xs = foldr mappend Nil xs

{-# INLINABLE length #-}
length :: List a -> Int
length xs = len xs 0
    where
        len Nil i = i
        len (x:.xs) i = len xs (i+1)

{-# INLINABLE head #-}
head Nil = undefined
head (x:.xs) = x

{-# INLINABLE last #-}
last Nil = undefined
last (x:.Nil) = x
last (x:.xs) = last xs

{-# INLINABLE take #-}
take 0 xs = Nil
take i Nil = Nil
take i (x:.xs) = x:.(take (i-1) xs)

{-# INLINABLE sum #-}
sum :: Num a => List a -> a
sum = foldl' (+) 0
