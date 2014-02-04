-- | A `SortedVector` is a vector that maintains the invariant that all elements are sorted. Whenever an element is added/removed, the vector is automatically adjusted. Because element positions can be changed in this way, it does not make sense to index the vector by specific locations.

module HLearn.DataStructures.SortedVector
    ( SortedVector
    )
    where
    
import Control.Applicative
import Control.DeepSeq
import Control.Monad.ST
import qualified Data.Foldable as F
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.List
import Debug.Trace
import GHC.TypeLits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG

import qualified Control.ConstraintKinds as CK

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype SortedVector a = SortedVector { vector :: V.Vector a}
    deriving (Read,Show,Eq,Ord)

bst2list :: SortedVector a -> [a]
bst2list (SortedVector vec) = V.toList vec

elem :: (Ord a) => a -> (SortedVector a) -> Bool
elem a (SortedVector vec) = go 0 (V.length vec - 1)
    where
        go lower upper
            | lower==upper = (vec V.! lower)==a
            | a > (vec V.! mid) = go (mid+1) upper
            | a < (vec V.! mid) = go lower (mid-1)
            | otherwise = True -- a==(vec V.! mid)
            where mid = floor $ (fromIntegral $ lower+upper)/2
   
instance (NFData a) => NFData (SortedVector a) where
    rnf (SortedVector v) = rnf v
    
-------------------------------------------------------------------------------
-- Algebra

instance Num dp => HasRing (SortedVector dp) where
    type Ring (SortedVector dp) = dp

instance Num dp => NumDP (SortedVector dp) where
    numdp (SortedVector v) = fromIntegral $ V.length v

instance (Ord a) => Abelian (SortedVector a)
instance (Ord a) => Monoid (SortedVector a) where
    {-# INLINE mempty #-}
    mempty = SortedVector $ V.empty
    
    {-# INLINE mappend #-}
    (SortedVector va) `mappend` (SortedVector vb) = SortedVector $ V.fromList $ merge2 (V.toList va) (V.toList vb)
        where
            merge2 xs [] = xs
            merge2 [] ys = ys
            merge2 (x:xs) (y:ys) =
                case compare x y of
                    LT -> x: merge2 xs (y:ys)
                    otherwise -> y: merge2 (x:xs) ys

instance (Ord a, Invertible a) => Group (SortedVector a) where
    {-# INLINE inverse #-}
    inverse (SortedVector vec) = SortedVector $ V.map mkinverse vec

---------------------------------------

instance CK.Foldable SortedVector where
    foldr f b (SortedVector vec) = V.foldr f b vec

instance F.Foldable SortedVector where
    foldr f b (SortedVector vec) = V.foldr f b vec

instance CK.Functor SortedVector where
    type FunctorConstraint SortedVector a = Ord a
    fmap f (SortedVector v) = SortedVector . V.fromList . sort . V.toList $ fmap f v

instance CK.Pointed SortedVector where
    point = SortedVector . V.singleton

-- instance CK.Applicative SortedVector where
--     (<*>) = undefined

instance CK.Monad SortedVector where
-- type MonadConstraint SortedVector a = Ord a
    return = SortedVector . V.singleton
    (>>=) = flip concatMapa

concatMapa :: (Ord a, Ord b) => (a -> SortedVector b) -> SortedVector a -> SortedVector b
concatMapa f v = reduce $ CK.fmap f v

join :: SortedVector (SortedVector a) -> SortedVector a
join = undefined

-------------------------------------------------------------------------------
-- Training

instance (Ord a) => HomTrainer (SortedVector a) where
    type Datapoint (SortedVector a) = a
    train1dp dp = SortedVector $ V.singleton dp
    train xs = SortedVector $ runST $ do
        let v = V.fromList $ F.toList xs
        vm <- V.unsafeThaw v
        Intro.sort vm
        V.freeze vm
--         runST $ do
--         v <- VG.fromList $ F.toList xs
-- --         Intro.sort v
--         V.freeze v
