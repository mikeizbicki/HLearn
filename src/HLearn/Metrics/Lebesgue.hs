module HLearn.Metrics.Lebesgue
    where

import Control.DeepSeq
import Data.Csv
import qualified Data.Vector.Unboxed as VU

import Test.QuickCheck

import HLearn.Algebra

-------------------------------------------------------------------------------
-- L1

newtype L1 a = L1 a
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

instance Num r => HasRing (L1 (VU.Vector r)) where
    type Ring (L1 (VU.Vector r)) = r

instance (VU.Unbox r, RealFrac r,Floating r) => MetricSpace (L1 (VU.Vector r)) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(L1 v1) !(L1 v2) = go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !(L1 v1) !(L1 v2) !dist = go 0 (VU.length v1-1) 
        where
            go tot (-1) = False 
            go tot i = if tot'>dist
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)

-------------------------------------------------------------------------------
-- L2

newtype L2 a = L2 a
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

instance Num r => HasRing (L2 (VU.Vector r)) where
    type Ring (L2 (VU.Vector r)) = r

-- instance (VU.Unbox r, RealFrac r,Floating r) => MetricSpace (L2 (VU.Vector r)) where
instance MetricSpace (L2 (VU.Vector Double)) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(L2 v1) !(L2 v2) = {-# SCC distance #-} sqrt $ go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !(L2 v1) !(L2 v2) !dist = {-# SCC isFartherThan #-} go 0 (VU.length v1-1) 
        where
            dist2=dist*dist

            go tot (-1) = False 
            go tot i = if tot'>dist2
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)

instance MetricSpace (L2 (VU.Vector Float)) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(L2 v1) !(L2 v2) = {-# SCC distance #-} sqrt $ go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !(L2 v1) !(L2 v2) !dist = {-# SCC isFartherThan #-} go 0 (VU.length v1-1) 
        where
            dist2=dist*dist

            go tot (-1) = False 
            go tot i = if tot'>dist2
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)

instance MkCentroid (L2 (VU.Vector Float)) where
    {-# INLINABLE mkCentroid #-}
    mkCentroid (L2 v1) (L2 v2) = {-# SCC mkCentroid #-} L2 $ VU.zipWith (\a b -> (a+b)/2) v1 v2

-------------------------------------------------------------------------------
-- Linf

newtype Linf a = Linf a
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

instance Num r => HasRing (Linf (VU.Vector r)) where
    type Ring (Linf (VU.Vector r)) = r

instance (VU.Unbox r, RealFrac r,Floating r) => MetricSpace (Linf (VU.Vector r)) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(Linf v1) !(Linf v2) = sqrt $ go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (max tot (v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !(Linf v1) !(Linf v2) !dist = go (VU.length v1-1) 
        where
            go (-1) = False 
            go i = if v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i > dist
                then True
                else go (i-1)

