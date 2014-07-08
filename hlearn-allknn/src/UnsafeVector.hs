{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, BangPatterns, StandaloneDeriving, GeneralizedNewtypeDeriving,FlexibleContexts #-}
module UnsafeVector
--     ( setptsize 
--     )
    where

import Control.DeepSeq
import Control.Monad
import Data.IORef
import qualified Data.Foldable as F
import Data.Primitive
import Data.Primitive.MachDeps
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe
import qualified Data.Strict.Maybe as Strict

import Data.Csv

import HLearn.Algebra
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- unsafe globals

{-# NOINLINE ptsizeIO #-}
ptsizeIO = unsafePerformIO $ newIORef (64::Int)

{-# NOINLINE ptalignIO #-}
ptalignIO = unsafePerformIO $ newIORef (64::Int)

-- {-# NOINLINE ptsize #-}
{-# INLINE ptsize #-}
ptsize = unsafePerformIO $ readIORef ptsizeIO

{-# NOINLINE ptalign #-}
ptalign = unsafePerformIO $ readIORef ptalignIO

setptsize :: Int -> IO ()
setptsize len = do
    writeIORef ptsizeIO len
    writeIORef ptalignIO len


-- x = VU.fromList [1..10::Float]
-- v = VU.fromList $ replicate 10 x

-------------------------------------------------------------------------------
-- vector opts

instance VUM.Unbox elem => VUM.Unbox (L2 VU.Vector elem)

data instance VUM.MVector s (L2 VU.Vector elem) = UnsafeMVector 
    { elemsizeM :: !Int
    , elemsizerealM :: !Int
    , lenM :: !Int
    , vecM :: !(VUM.MVector s elem) 
    }

instance 
    ( VG.Vector VU.Vector elem
    , VGM.MVector VUM.MVector elem
    ) => VGM.MVector VUM.MVector (L2 VU.Vector elem) 
        where
    {-# INLINE basicLength #-}
    basicLength uv = lenM uv

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' uv = UnsafeMVector
        { elemsizeM = elemsizeM uv
        , elemsizerealM = elemsizerealM uv
        , lenM = lenM'
        , vecM = VGM.basicUnsafeSlice (i*elemsizerealM uv) (lenM'*elemsizerealM uv) $ vecM uv
        }

    {-# INLINE basicOverlaps #-}
    basicOverlaps uv1 uv2 = VGM.basicOverlaps (vecM uv1) (vecM uv2)

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew lenM' = do
        let elemsizeM'=ptsize
        let elemsizerealM'=ptalign*(ptsize `mod` ptalign)+ptalign
        vecM' <- VGM.basicUnsafeNew (lenM'*elemsizerealM')
        return $ UnsafeMVector
            { elemsizeM=elemsizeM'
            , elemsizerealM=elemsizerealM'
            , lenM=lenM'
            , vecM=vecM'
            }

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead uv i = 
        liftM L2 $ VG.freeze $ VGM.unsafeSlice (i*elemsizerealM uv) (elemsizeM uv) (vecM uv)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite uv loc v =
        forM_ [0..elemsizeM uv-1] $ \i -> do
            let x = v `VG.unsafeIndex` i
            VGM.unsafeWrite (vecM uv) (start+i) x
        where
            start = loc*elemsizerealM uv

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy v1 v2 = VGM.basicUnsafeCopy (vecM v1) (vecM v2)
-- 
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove v1 v2 = VGM.basicUnsafeMove (vecM v1) (vecM v2)

--     {-# INLINE basicSet #-}
--     basicSet v x = VGM.basicSet (vecM v) x

-------------------------------------------------------------------------------
-- immutable vector

data instance VU.Vector (L2 VU.Vector elem) = UnsafeVector 
    { elemsize :: !Int
    , elemsizereal :: !Int
    , len :: !Int
    , vec :: !(L2 VU.Vector elem) 
    }

instance 
    ( VG.Vector VU.Vector elem
    , VGM.MVector VUM.MVector elem
    ) => VG.Vector VU.Vector (L2 VU.Vector elem) 
        where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze uv = do
        vec' <- VG.basicUnsafeFreeze (vecM uv)
        return $ UnsafeVector
            { elemsize = elemsizeM uv
            , elemsizereal = elemsizerealM uv
            , len = lenM uv
            , vec = L2 vec'
            }

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw uv = do
        vecM' <- VG.basicUnsafeThaw (unL2 $ vec uv)
        return $ UnsafeMVector
            { elemsizeM = elemsize uv
            , elemsizerealM = elemsizereal uv
            , lenM = len uv
            , vecM = vecM'
            }
    
    {-# INLINE basicLength #-}
    basicLength uv = len uv

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i len' uv = uv
        { len = len'
        , vec = VG.basicUnsafeSlice (i*elemsizereal uv) (len'*elemsizereal uv) (vec uv)
        }

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM uv i = return $ VG.basicUnsafeSlice (i*elemsizereal uv) (elemsize uv) (vec uv)

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy mv v = VG.basicUnsafeCopy (vecM mv) (vec v)

-------------------------------------------------------------------------------
-- L2'

newtype L2' v a = L2' { unL2' :: v a }
    deriving (Read,Show,Eq,Ord,FromRecord,NFData)

deriving instance F.Foldable v => F.Foldable (L2' v)
deriving instance Functor v => Functor (L2' v)

instance VG.Vector v a => VG.Vector (L2' v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (L2'M v) = liftM L2' $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (L2' v) = liftM L2'M $ VG.basicUnsafeThaw v
    basicLength (L2' v) = VG.basicLength v
    basicUnsafeSlice s t (L2' v) = L2' $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (L2' v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (L2'M vm) (L2' v) = VG.basicUnsafeCopy vm v
    elemseq (L2' v) a b = VG.elemseq v a b

newtype L2'M v s a = L2'M { unL2'M :: v s a } 

instance VGM.MVector v a => VGM.MVector (L2'M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (L2'M v) = VGM.basicLength v
    basicUnsafeSlice s t (L2'M v) = L2'M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (L2'M v1) (L2'M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM L2'M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM L2'M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (L2'M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (L2'M v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (L2'M v1) (L2'M v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (L2'M v1) (L2'M v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (L2'M v1) i = L2'M `liftM` VGM.basicUnsafeGrow v1 i

type instance VG.Mutable (L2' v) = L2'M (VG.Mutable v)

---------------------------------------

type instance Scalar (L2' v r) = r

instance (VG.Vector v r, RealFrac r, Floating r) => MetricSpace (L2' v r) where
-- instance MetricSpace (L2' VU.Vector Float) where

    {-# INLINE distance #-}
    distance !(L2' v1) !(L2' v2) = {-# SCC distance #-} sqrt $ go 0 (ptsize-1)
        where
            go !tot (-1) = tot
            go !tot !i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError !(L2' v1) !(L2' v2) !dist = {-# SCC isFartherThanWithDistanceMonoCanError #-} 
        go 0 0
        where
            dist2=dist*dist

            ptsize=20
--             ptsize=VG.length v1

            {-# INLINE goEach #-}
            goEach !tot !i = if i>= ptsize 
                then tot
                else if tot'>dist2
                    then errorVal
                    else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

--             go !tot !i = if tot'>dist2
--                     then errorVal
--                     else if i>=ptsize-8
--                         then goEach tot i
--                         else go tot' (i+8)
            go !tot !i = if tot'>dist2
                then errorVal
                else if i>=ptsize-8
                    then sqrt $ goEach tot' (i+8) 
                    else go tot' (i+8)
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
                        +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                        *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                        +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                        *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                        +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                        *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                        +(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
                        *(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
--
--                         +(v1 `VG.unsafeIndex` (i+8)-v2 `VG.unsafeIndex` (i+8))
--                         *(v1 `VG.unsafeIndex` (i+8)-v2 `VG.unsafeIndex` (i+8))
--                         +(v1 `VG.unsafeIndex` (i+9)-v2 `VG.unsafeIndex` (i+9))
--                         *(v1 `VG.unsafeIndex` (i+9)-v2 `VG.unsafeIndex` (i+9))
--                         +(v1 `VG.unsafeIndex` (i+10)-v2 `VG.unsafeIndex` (i+10))
--                         *(v1 `VG.unsafeIndex` (i+10)-v2 `VG.unsafeIndex` (i+10))
--                         +(v1 `VG.unsafeIndex` (i+11)-v2 `VG.unsafeIndex` (i+11))
--                         *(v1 `VG.unsafeIndex` (i+11)-v2 `VG.unsafeIndex` (i+11))
--                         +(v1 `VG.unsafeIndex` (i+12)-v2 `VG.unsafeIndex` (i+12))
--                         *(v1 `VG.unsafeIndex` (i+12)-v2 `VG.unsafeIndex` (i+12))
--                         +(v1 `VG.unsafeIndex` (i+13)-v2 `VG.unsafeIndex` (i+13))
--                         *(v1 `VG.unsafeIndex` (i+13)-v2 `VG.unsafeIndex` (i+13))
--                         +(v1 `VG.unsafeIndex` (i+14)-v2 `VG.unsafeIndex` (i+14))
--                         *(v1 `VG.unsafeIndex` (i+14)-v2 `VG.unsafeIndex` (i+14))
--                         +(v1 `VG.unsafeIndex` (i+15)-v2 `VG.unsafeIndex` (i+15))
--                         *(v1 `VG.unsafeIndex` (i+15)-v2 `VG.unsafeIndex` (i+15))
--                         +(v1 `VG.unsafeIndex` (i+16)-v2 `VG.unsafeIndex` (i+16))
--                         *(v1 `VG.unsafeIndex` (i+16)-v2 `VG.unsafeIndex` (i+16))
--                         +(v1 `VG.unsafeIndex` (i+17)-v2 `VG.unsafeIndex` (i+17))
--                         *(v1 `VG.unsafeIndex` (i+17)-v2 `VG.unsafeIndex` (i+17))
--                         +(v1 `VG.unsafeIndex` (i+18)-v2 `VG.unsafeIndex` (i+18))
--                         *(v1 `VG.unsafeIndex` (i+18)-v2 `VG.unsafeIndex` (i+18))
--                         +(v1 `VG.unsafeIndex` (i+19)-v2 `VG.unsafeIndex` (i+19))
--                         *(v1 `VG.unsafeIndex` (i+19)-v2 `VG.unsafeIndex` (i+19))

            {-# INLINE goSmall #-}
            goSmall !tot !i = goEach tot i
--             goSmall !tot !i = if diff <5
--                 then if diff <3
--                     then if diff == 1
--                         then go1 tot i
--                         else go2 tot i
--                     else if diff == 3
--                         then go3 tot i
--                         else go4 tot i 
--                 else if diff < 7
--                     then if diff == 5
--                         then go5 tot i
--                         else go6 tot i
--                     else if diff == 7
--                         then go7 tot i
--                         else go8 tot i
--                 where
--                     diff = VG.length v1-i

            {-# INLINE go1 #-}
            {-# INLINE go2 #-}
            {-# INLINE go3 #-}
            {-# INLINE go4 #-}
            {-# INLINE go5 #-}
            {-# INLINE go6 #-}
            {-# INLINE go7 #-}
            {-# INLINE go8 #-}
            go1 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
            go2 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
            go3 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
            go4 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
            go5 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
            go6 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
            go7 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
            go8 !tot !i = tot
                +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                +(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
                *(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
                              
