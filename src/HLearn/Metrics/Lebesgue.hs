module HLearn.Metrics.Lebesgue
    where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Csv
import Data.Primitive.MutVar
import qualified Data.Foldable as F
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import System.IO.Unsafe

import Test.QuickCheck

-- import Data.SIMD
import HLearn.Algebra

-------------------------------------------------------------------------------
-- L1

newtype L1 v a = L1 { unL1 :: v a }
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

deriving instance F.Foldable v => F.Foldable (L1 v)
deriving instance Functor v => Functor (L1 v)

instance VG.Vector v a => VG.Vector (L1 v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (L1M v) = liftM L1 $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (L1 v) = liftM L1M $ VG.basicUnsafeThaw v
    basicLength (L1 v) = VG.basicLength v
    basicUnsafeSlice s t (L1 v) = L1 $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (L1 v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (L1M vm) (L1 v) = VG.basicUnsafeCopy vm v
    elemseq (L1 v) a b = VG.elemseq v a b

newtype L1M v s a = L1M { unL1M :: v s a } 

instance VGM.MVector v a => VGM.MVector (L1M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (L1M v) = VGM.basicLength v
    basicUnsafeSlice s t (L1M v) = L1M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (L1M v1) (L1M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM L1M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM L1M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (L1M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (L1M v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (L1 v) = L1M (VG.Mutable v)

---------------------------------------

type instance Scalar (L1 v r) = r

instance (VG.Vector v r, RealFrac r, Floating r) => MetricSpace (L1 v r) where
-- instance (VG.Unbox r, RealFrac r,Floating r) => MetricSpace (L1 VG.Vector r) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(L1 v1) !(L1 v2) = go 0 (VG.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)
--             go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
--                               *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

    isFartherThan !(L1 v1) !(L1 v2) !dist = go 0 (VG.length v1-1) 
        where
            go tot (-1) = False 
            go tot i = if tot'>dist
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

-------------------------------------------------------------------------------
-- L2

newtype L2 v a = L2 { unL2 :: v a }
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

deriving instance F.Foldable v => F.Foldable (L2 v)
deriving instance Functor v => Functor (L2 v)

instance VG.Vector v a => VG.Vector (L2 v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (L2M v) = liftM L2 $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (L2 v) = liftM L2M $ VG.basicUnsafeThaw v
    basicLength (L2 v) = VG.basicLength v
    basicUnsafeSlice s t (L2 v) = L2 $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (L2 v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (L2M vm) (L2 v) = VG.basicUnsafeCopy vm v
    elemseq (L2 v) a b = VG.elemseq v a b

newtype L2M v s a = L2M { unL2M :: v s a } 

instance VGM.MVector v a => VGM.MVector (L2M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (L2M v) = VGM.basicLength v
    basicUnsafeSlice s t (L2M v) = L2M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (L2M v1) (L2M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM L2M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM L2M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (L2M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (L2M v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (L2M v1) (L2M v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (L2M v1) (L2M v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (L2M v) i = L2M `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (L2 v) = L2M (VG.Mutable v)

---------------------------------------

type instance Scalar (L2 v r) = r

instance (VG.Vector v r, RealFrac r, Floating r) => MetricSpace (L2 v r) where
-- instance MetricSpace (L2 VU.Vector Float) where

--     {-# INLINE distance #-}
--     distance (L2 v1) (L2 v2) = sqrt $ plusHorizontalX4 $ go 0 (VG.length v1'-1)
--         where
--             v1' = unsafeVectorizeUnboxedX4 v1
--             v2' = unsafeVectorizeUnboxedX4 v2
--             
--             go tot (-1) = tot
--             go tot i = go tot' (i-1)
--                 where
--                     tot' = tot+diff*diff
--                     diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i
-- 
--             goEach !tot !i = if i>= VG.length v1
--                 then tot
--                 else goEach tot' (i+1)
--                 where
--                     tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
--                               *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

    {-# INLINE distance #-}
    distance !(L2 v1) !(L2 v2) = {-# SCC distance #-} sqrt $ goEach 0 0
        where
            go !tot !i = {-# SCC distance_go #-} if i>VG.length v1-4
                then goEach tot i
                else go tot' (i+4)
                where
                    tot' = tot
                        +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
--                         +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
--                         *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
--                         +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
--                         *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
--                         +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
--                         *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
--                         +(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
--                         *(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))

            goEach !tot !i = if i>= VG.length v1
                then tot
                else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError (L2 v1) (L2 v2) !dist = {-# SCC isFartherThanWithDistanceCanError #-} 
        sqrt $ go 0 0
        where
            dist2=dist*dist

            go !tot !i = if i>VG.length v1-4
                then goEach tot i
                else if tot'>dist2
                    then errorVal
                    else go tot' (i+4)
                where
                    tot' = tot
                        +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
--                         +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
--                         *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
--                         +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
--                         *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
--                         +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
--                         *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
--                         +(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
--                         *(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))

            goEach !tot !i = if i>= VG.length v1
                then tot
                else if tot'>dist2
                    then errorVal
                    else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

-------------------------------------------------------------------------------
-- Linf

newtype Linf v a = Linf { unLinf :: v a }
    deriving (Read,Show,Eq,Ord,Arbitrary,FromRecord,NFData)

deriving instance F.Foldable v => F.Foldable (Linf v)
deriving instance Functor v => Functor (Linf v)

instance VG.Vector v a => VG.Vector (Linf v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (LinfM v) = liftM Linf $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Linf v) = liftM LinfM $ VG.basicUnsafeThaw v
    basicLength (Linf v) = VG.basicLength v
    basicUnsafeSlice s t (Linf v) = Linf $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Linf v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (LinfM vm) (Linf v) = VG.basicUnsafeCopy vm v
    elemseq (Linf v) a b = VG.elemseq v a b

newtype LinfM v s a = LinfM { unLinfM :: v s a } 

instance VGM.MVector v a => VGM.MVector (LinfM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (LinfM v) = VGM.basicLength v
    basicUnsafeSlice s t (LinfM v) = LinfM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (LinfM v1) (LinfM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM LinfM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM LinfM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (LinfM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (LinfM v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (Linf v) = LinfM (VG.Mutable v)

---------------------------------------

type instance Scalar (Linf v r) = r

instance (VG.Vector v r, RealFrac r, Floating r) => MetricSpace (Linf v r) where
-- instance (VG.Unbox r, RealFrac r,Floating r) => MetricSpace (Linf VG.Vector r) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(Linf v1) !(Linf v2) = go 0 (VG.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(max (v1 `VG.unsafeIndex` i) (v2 `VG.unsafeIndex` i))) (i-1)
--             go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
--                               *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

    isFartherThan !(Linf v1) !(Linf v2) !dist = go 0 (VG.length v1-1) 
        where
            go tot (-1) = False 
            go tot i = if tot'>dist
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(max (v1 `VG.unsafeIndex` i) (v2 `VG.unsafeIndex` i))

---------------------------------------

-- instance (MetricSpace (v r), VG.Vector v r, Fractional r) => MkCentroid (v r) where
--     {-# INLINABLE mkCentroid #-}
--     mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2

instance MkCentroid (L1 VU.Vector Double) where
    {-# INLINABLE mkCentroid #-}
    mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2

-- instance MkCentroid (L2 VU.Vector Double) where
--     {-# INLINABLE mkCentroid #-}
--     mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2

instance MkCentroid (Linf VU.Vector Double) where
    {-# INLINABLE mkCentroid #-}
    mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2


instance MkCentroid (L1 VU.Vector Float) where
    {-# INLINABLE mkCentroid #-}
    mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2

instance MkCentroid (L2 VU.Vector Float) where
    {-# INLINABLE mkCentroid #-}
    mkCentroid v1 v2 = {-# SCC mkCentroid #-} VG.zipWith (\a b -> (a+b)/2) v1 v2


