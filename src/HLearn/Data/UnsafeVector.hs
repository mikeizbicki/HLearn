module HLearn.Data.UnsafeVector
    ( setptsize
    )
    where

import Control.DeepSeq
import Control.Monad
import Data.IORef
import Debug.Trace
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

import Unsafe.Coerce
import Data.Hashable
import Data.Primitive.ByteArray
import GHC.Prim
import GHC.Int
import GHC.Types

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return, liftM, forM_)
-- import SubHask.Compatibility.Vector.HistogramMetrics
import SubHask.Compatibility.Vector.Lebesgue

-------------------------------------------------------------------------------
-- unsafe globals

{-# NOINLINE ptsizeIO #-}
ptsizeIO = unsafeDupablePerformIO $ newIORef (16::Int)

{-# NOINLINE ptalignIO #-}
ptalignIO = unsafeDupablePerformIO $ newIORef (16::Int)

{-# NOINLINE ptsize #-}
ptsize = unsafeDupablePerformIO $ readIORef ptsizeIO

{-# NOINLINE ptalign #-}
ptalign = unsafeDupablePerformIO $ readIORef ptalignIO

setptsize :: Int -> IO ()
setptsize len = do
    writeIORef ptsizeIO len
    writeIORef ptalignIO (4::Int)

-------------------------------------------------------------------------------
-- l2 vector ops

instance VUM.Unbox elem => VUM.Unbox (L2 VU.Vector elem)

data instance VUM.MVector s (L2 VU.Vector elem) = UnsafeMVector
    { elemsizeM     :: {-#UNPACK#-} !Int
    , elemsizerealM :: {-#UNPACK#-} !Int
    , lenM          :: {-#UNPACK#-} !Int
    , vecM          :: !(VUM.MVector s elem)
    }

instance
    ( VG.Vector VU.Vector elem
    , VGM.MVector VUM.MVector elem
    ) => VGM.MVector VUM.MVector (L2 VU.Vector elem)
        where
    {-# INLINABLE basicLength #-}
    basicLength uv = lenM uv

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' uv = UnsafeMVector
        { elemsizeM = elemsizeM uv
        , elemsizerealM = elemsizerealM uv
        , lenM = lenM'
        , vecM = VGM.basicUnsafeSlice (i*elemsizerealM uv) (lenM'*elemsizerealM uv) $ vecM uv
        }

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps uv1 uv2 = VGM.basicOverlaps (vecM uv1) (vecM uv2)

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew lenM' = do
        let elemsizeM'=ptsize
--         let elemsizerealM'=20
        let elemsizerealM'=ptalign*(ptsize `div` ptalign)
                          +if ptsize `mod` ptalign == 0 then 0 else ptalign

--         trace ("elemsizeM'     = "++show elemsizeM') $ return ()
--         trace ("elemsizerealM' = "++show elemsizerealM') $ return ()

        vecM' <- VGM.basicUnsafeNew (lenM'*elemsizerealM')
        return $ UnsafeMVector
            { elemsizeM=elemsizeM'
            , elemsizerealM=elemsizerealM'
            , lenM=lenM'
            , vecM=vecM'
            }

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead uv i =
        liftM L2 $ VG.freeze $ VGM.unsafeSlice (i*elemsizerealM uv) (elemsizeM uv) (vecM uv)

    {-# INLINABLE basicUnsafeWrite #-}
--     FIXME: this is probably better, but it causes GHC to panic
--     basicUnsafeWrite uv loc v = go 0
--         where
--             go i = if i <= elemsizerealM uv
--                 then do
--                     VGM.unsafeWrite (vecM uv) (start+i) $ v `VG.unsafeIndex` i
--                     go (i+1)
--                 else return ()
--             start = loc*elemsizerealM uv

    basicUnsafeWrite uv loc v =
        forM_ [0..elemsizeM uv-1] $ \i -> do
            let x = v `VG.unsafeIndex` i
            VGM.unsafeWrite (vecM uv) (start+i) x
        where
            start = loc*elemsizerealM uv

    {-# INLINABLE basicUnsafeCopy #-}
    basicUnsafeCopy v1 v2 = VGM.basicUnsafeCopy (vecM v1) (vecM v2)

    {-# INLINABLE basicUnsafeMove #-}
    basicUnsafeMove v1 v2 = VGM.basicUnsafeMove (vecM v1) (vecM v2)

--     {-# INLINABLE basicSet #-}
--     basicSet v x = VGM.basicSet (vecM v) x

-------------------------------------------------------------------------------
-- immutable vector

data instance VU.Vector (L2 VU.Vector elem) = UnsafeVector
    { elemsize     :: {-# UNPACK #-} !Int
    , elemsizereal :: {-# UNPACK #-} !Int
    , len          :: {-# UNPACK #-} !Int
    , vec          :: !(L2 VU.Vector elem)
    }

instance
    ( VG.Vector VU.Vector elem
    , VGM.MVector VUM.MVector elem
    ) => VG.Vector VU.Vector (L2 VU.Vector elem)
        where

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze uv = do
        vec' <- VG.basicUnsafeFreeze (vecM uv)
        return $ UnsafeVector
            { elemsize = elemsizeM uv
            , elemsizereal = elemsizerealM uv
            , len = lenM uv
            , vec = L2 vec'
            }

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw uv = do
        vecM' <- VG.basicUnsafeThaw (unL2 $ vec uv)
        return $ UnsafeMVector
            { elemsizeM = elemsize uv
            , elemsizerealM = elemsizereal uv
            , lenM = len uv
            , vecM = vecM'
            }

    {-# INLINABLE basicLength #-}
    basicLength uv = len uv

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i len' uv = uv
        { len = len'
        , vec = VG.basicUnsafeSlice (i*elemsizereal uv) (len'*elemsizereal uv) (vec uv)
        }

    {-# INLINABLE basicUnsafeIndexM #-}
    basicUnsafeIndexM uv i = return $ VG.basicUnsafeSlice (i*elemsizereal uv) (elemsize uv) (vec uv)

--     {-# INLINABLE basicUnsafeCopy #-}
--     basicUnsafeCopy mv v = VG.basicUnsafeCopy (vecM mv) (vec v)

-------------------------------------------------------------------------------
-- Labeled'

instance (VUM.Unbox x, VUM.Unbox y) => VUM.Unbox (Labeled' x y)

newtype instance VUM.MVector s (Labeled' x y) = UMV_Labeled' (VUM.MVector s (x,y))

instance
    ( VUM.Unbox x
    , VUM.Unbox y
    ) => VGM.MVector VUM.MVector (Labeled' x y)
        where

    {-# INLINABLE basicLength #-}
    {-# INLINABLE basicUnsafeSlice #-}
    {-# INLINABLE basicOverlaps #-}
    {-# INLINABLE basicUnsafeNew #-}
    {-# INLINABLE basicUnsafeRead #-}
    {-# INLINABLE basicUnsafeWrite #-}
    {-# INLINABLE basicUnsafeCopy #-}
    {-# INLINABLE basicUnsafeMove #-}
    {-# INLINABLE basicSet #-}
    basicLength (UMV_Labeled' v) = VGM.basicLength v
    basicUnsafeSlice i len (UMV_Labeled' v) = UMV_Labeled' $ VGM.basicUnsafeSlice i len v
    basicOverlaps (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew len = liftM UMV_Labeled' $ VGM.basicUnsafeNew len
    basicUnsafeRead (UMV_Labeled' v) i = do
        (!x,!y) <- VGM.basicUnsafeRead v i
        return $ Labeled' x y
    basicUnsafeWrite (UMV_Labeled' v) i (Labeled' x y) = VGM.basicUnsafeWrite v i (x,y)
    basicUnsafeCopy (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicUnsafeMove v1 v2
    basicSet (UMV_Labeled' v1) (Labeled' x y) = VGM.basicSet v1 (x,y)

newtype instance VU.Vector (Labeled' x y) = UV_Labeled' (VU.Vector (x,y))

instance
    ( VUM.Unbox x
    , VUM.Unbox y
    ) => VG.Vector VU.Vector (Labeled' x y)
        where

    {-# INLINABLE basicUnsafeFreeze #-}
    {-# INLINABLE basicUnsafeThaw #-}
    {-# INLINABLE basicLength #-}
    {-# INLINABLE basicUnsafeSlice #-}
    {-# INLINABLE basicUnsafeIndexM #-}
    basicUnsafeFreeze (UMV_Labeled' v) = liftM UV_Labeled' $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (UV_Labeled' v) = liftM UMV_Labeled' $ VG.basicUnsafeThaw v
    basicLength (UV_Labeled' v) = VG.basicLength v
    basicUnsafeSlice i len (UV_Labeled' v) = UV_Labeled' $ VG.basicUnsafeSlice i len v
    basicUnsafeIndexM (UV_Labeled' v) i = do
        (!x,!y) <- VG.basicUnsafeIndexM v i
        return $ Labeled' x y
