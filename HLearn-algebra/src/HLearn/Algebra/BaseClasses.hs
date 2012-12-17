{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Algebra.BaseClasses
    where

import GHC.Prim

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G

import qualified Prelude as P

-------------------------------------------------------------------------------
-- Functor

class Functor f where
    type FunctorConstraint f x :: Constraint
    type FunctorConstraint f x = ()

    fmap :: (FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> f a -> f b

instance Functor [] where
    {-# INLINE fmap #-}
    fmap = P.map

instance Functor V.Vector where
    {-# INLINE fmap #-}
    fmap = V.map

instance Functor VU.Vector where
    type FunctorConstraint VU.Vector x = VU.Unbox x
    {-# INLINE fmap #-}
    fmap = VU.map

-------------------------------------------------------------------------------
-- Foldable

class Foldable f where
    type FoldableConstraint f x :: Constraint
    type FoldableConstraint f x = ()
    
    foldl' :: (FoldableConstraint f a, FoldableConstraint f b) => (a -> b -> a) -> a -> f b -> a
    
instance Foldable [] where
    {-# INLINE foldl' #-}
    foldl' = L.foldl'
    
instance Foldable V.Vector where
    {-# INLINE foldl' #-}
    foldl' = V.foldl'

instance Foldable VU.Vector where
    type FoldableConstraint VU.Vector x = VU.Unbox x
    {-# INLINE foldl' #-}
    foldl' = VU.foldl'


-------------------------------------------------------------------------------
-- Traversable

-- class Foldable f where
--     type FoldableConstraint f x :: Constraint
--     type FoldableConstraint f x = ()
--     
--     foldl' :: (FoldableConstraint f a, FoldableConstraint f b) => (a -> b -> a) -> a -> f b -> a
--     
-- instance Foldable [] where
--     foldl' = L.foldl'
--     
-- instance Foldable V.Vector where
--     type FoldableConstraint V.Vector x = VU.Unbox x
--     foldl' = V.foldl'
-- 
-- instance Foldable VU.Vector where
--     type FoldableConstraint VU.Vector x = VU.Unbox x
--     foldl' = VU.foldl'
-- 

foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc xs = runST $ do
    acc' <- newSTRef acc            -- Create a variable for the accumulator
    forM_ xs $ \x -> do             -- For each x in xs...
        a <- readSTRef acc'         -- read the accumulator
        writeSTRef acc' (f a x)     -- apply f to the accumulator and x
    readSTRef acc'                  -- and finally read the result
