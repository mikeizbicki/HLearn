{-# LANGUAGE EmptyDataDecls #-}
-- | A `SortedVector'` is a vector that maintains the invariant that all elements are sorted.  Whenever an element is added/removed, the vector is automatically adjusted.  Because element positions can be changed in this way, it does not make sense to index the vector by specific locations.

module HLearn.DataStructures.SortedVector
    ( SortedVector, SortedVector' (..)
    )
    where
    
import Control.Applicative
import Control.DeepSeq
import Control.Monad.ST
import qualified Data.Foldable as F
import Data.List
import Debug.Trace
import GHC.TypeLits
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as Intro

import qualified Control.ConstraintKinds as CK

import Data.Prunable
import HLearn.Algebra 

-------------------------------------------------------------------------------
-- data types

data AnySort

type SortedVector dp = SortedVector' V.Vector AnySort dp

newtype SortedVector' (vec :: * -> *) sort dp = SortedVector' { vector :: vec dp}
    deriving (Read,Show,Eq,Ord)

bst2list :: (VG.Vector vec dp) => SortedVector' vec sort dp -> [dp]
bst2list (SortedVector' vec) = VG.toList vec

elem :: (VG.Vector vec dp, Ord dp) => dp -> (SortedVector' vec sort dp) -> Bool
elem a (SortedVector' vec) = go 0 (VG.length vec - 1)
    where
        go lower upper
            | lower==upper      = (vec VG.! lower)==a
            | a > (vec VG.! mid) = go (mid+1) upper
            | a < (vec VG.! mid) = go lower (mid-1)
            | otherwise         = True -- a==(vec VG.! mid)
            where mid = floor $ (fromIntegral $ lower+upper)/2
   
instance (NFData (vec dp)) => NFData (SortedVector' vec sort dp) where
    rnf (SortedVector' v) = rnf v
    
-------------------------------------------------------------------------------
-- Algebra

instance (VG.Vector vec dp, Ord dp) => Abelian (SortedVector' vec sort dp)
instance (VG.Vector vec dp, Ord dp) => Monoid (SortedVector' vec sort dp) where
    {-# INLINE mempty #-}
    mempty = SortedVector' $ VG.empty
    
    {-# INLINE mappend #-}
    (SortedVector' va) `mappend` (SortedVector' vb) = SortedVector' $ VG.fromList $ merge2 (VG.toList va) (VG.toList vb)
        where
            merge2 xs [] = xs
            merge2 [] ys = ys
            merge2 (x:xs) (y:ys) =
                case compare x y of
                    LT        -> x: merge2 xs (y:ys)
                    otherwise -> y: merge2 (x:xs) ys

instance (VG.Vector vec dp, Ord dp, Invertible dp) => Group (SortedVector' vec sort dp) where
    {-# INLINE inverse #-}
    inverse (SortedVector' vec) = SortedVector' $ VG.map mkinverse vec

---------------------------------------

instance (VG.Vector vec dp) => Index (SortedVector' vec sort dp) where
    type IndexType (SortedVector' vec sort dp) = TreeIndex
    type IndexResult (SortedVector' vec sort dp) = SortedVector' vec sort dp
    (!) (SortedVector' vec) TreeLeft  = SortedVector' $ VG.take (floor $ (fromIntegral $ VG.length $ vec)/2) $ vec
    (!) (SortedVector' vec) TreeRight = SortedVector' $ VG.drop (floor $ (fromIntegral $ VG.length $ vec)/2) $ vec

-- instance (VG.Vector vec dp) => Prunable (SortedVector' vec sort) where
--     prunefoldr p f b v@(SortedVector' vec)
--         | VG.length vec == 1 = f (vec VG.! 0) b
--         | otherwise = if p b (SortedVector' vec) TreeLeft
--             then goright 
--             else prunefoldr p f goright (v ! TreeLeft)
-- 
--             where 
--                 goright = if p b (SortedVector' vec) TreeRight
--                     then b
--                     else prunefoldr p f b (v ! TreeRight)
-- 
-- search_cata :: (Eq dp) => dp -> dp -> Bool -> Bool
-- search_cata query dp bool = query==dp || bool
-- 
-- search_prune :: (VG.Vector vec dp, Ord dp) => dp -> Bool -> SortedVector' vec sort dp -> TreeIndex -> Bool
-- search_prune query _ v TreeLeft  = (vector v) VG.! (floor $ (fromIntegral $ VG.length $ vector v)/2) < query
-- search_prune query _ v TreeRight = (vector v) VG.! (floor $ (fromIntegral $ VG.length $ vector v)/2) > query
-- 
-- binarySearch :: (VG.Vector vec dp, Ord dp) => dp -> SortedVector' vec sort dp -> Bool
-- binarySearch query sv = prunefoldr (search_prune query) (search_cata query) False sv
-- 
-- instance F.Foldable (SortedVector' vec sort) where
--     foldr f b (SortedVector' vec) = VG.foldr f b vec

instance CK.Foldable (SortedVector' vec sort) where
    type FoldableConstraint (SortedVector' vec sort) dp = VG.Vector vec dp
    foldr f b (SortedVector' vec) = VG.foldr f b vec
    foldr1 f (SortedVector' vec) = VG.foldr1 f vec

instance CK.Functor (SortedVector' vec sort) where
    type FunctorConstraint (SortedVector' vec sort) dp = (Ord dp, VG.Vector vec dp, Functor vec)
--     fmap f (SortedVector' v) = SortedVector' . VG.fromList . sort . VG.toList $ fmap f v
    fmap f (SortedVector' v) = SortedVector' $ runST $ do
        v <- VG.thaw $ fmap f v
        Intro.sort v
        v <- VG.freeze v
        return v

instance CK.Pointed (SortedVector' vec sort) where
    point = SortedVector' . VG.singleton

-- instance CK.Monad (SortedVector' vec sort) where
-- --     type MonadConstraint SortedVector' a = Ord a
--     return = SortedVector' . VG.singleton 
--     (>>=) = flip concatMapa
-- 
-- concatMapa :: 
--     ( Ord a, Ord b, VG.Vector vec b
--     ) => (a -> SortedVector' vec sort b) -> SortedVector' vec sort a -> SortedVector' vec sort b
-- concatMapa f v = reduce $ CK.fmap f v

-------------------------------------------------------------------------------
-- Training

instance (VG.Vector vec dp, Ord dp) => HomTrainer (SortedVector' vec sort dp) where
    type Datapoint (SortedVector' vec sort dp) = dp
    train1dp dp = SortedVector' $ VG.singleton dp
    train xs = SortedVector' $ runST $ do
        v <- VG.thaw $ (VG.fromList $ F.toList xs :: vec dp)
        Intro.sort v
        v <- VG.freeze v
        return v
