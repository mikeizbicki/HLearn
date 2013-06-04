module HLearn.DataStructures.BinarySearchTree
    where
    
import qualified Data.Foldable as F
import Data.List
import Debug.Trace
import GHC.TypeLits

import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- data types

newtype BST a = BST (V.Vector a)
    deriving (Read,Show,Eq,Ord)

bst2list :: BST a -> [a]
bst2list (BST vec) = V.toList vec

elem :: (Ord a) => a -> (BST a) -> Bool
elem a (BST vec) = go 0 (V.length vec - 1)
    where
        go lower upper
            | lower==upper      = (vec V.! lower)==a
            | a > (vec V.! mid) = go (mid+1) upper
            | a < (vec V.! mid) = go lower (mid-1)
            | otherwise         = True -- a==(vec V.! mid)
            where mid = floor $ (fromIntegral $ lower+upper)/2
    
-------------------------------------------------------------------------------
-- Algebra

instance (Ord a) => Monoid (BST a) where
    {-# INLINE mempty #-}
    mempty = BST $ V.empty
    
    {-# INLINE mappend #-}
    (BST va) `mappend` (BST vb) = BST $ V.fromList $ merge2 (V.toList va) (V.toList vb)
        where
            merge2 xs [] = xs
            merge2 [] ys = ys
            merge2 (x:xs) (y:ys) =
                case compare x y of
                    LT        -> x: merge2 xs (y:ys)
                    otherwise -> y: merge2 (x:xs) ys

instance (Ord a, Invertible a) => Group (BST a) where
    {-# INLINE inverse #-}
    inverse (BST vec) = BST $ V.map mkinverse vec

instance F.Foldable BST where
    foldr f b (BST vec) = V.foldr f b vec

-------------------------------------------------------------------------------
-- Training

instance (Ord a) => HomTrainer (BST a) where
    type Datapoint (BST a) = a
    train1dp dp = BST $ V.singleton dp
