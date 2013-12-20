{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module UnsafeVector
--     ( setptsize 
--     )
    where

import Control.Monad
import Data.IORef
import Data.Primitive
import Data.Primitive.MachDeps
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe

import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- unsafe globals

{-# NOINLINE ptsizeIO #-}
ptsizeIO = unsafePerformIO $ newIORef (64::Int)

{-# NOINLINE ptalignIO #-}
ptalignIO = unsafePerformIO $ newIORef (64::Int)

{-# NOINLINE ptsize #-}
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

instance VUM.Unbox (L2 VU.Vector Float)

data instance VUM.MVector s (L2 VU.Vector Float) = UnsafeMVector 
    { elemsizeM :: !Int
    , elemsizerealM :: !Int
    , lenM :: !Int
    , vecM :: !(VUM.MVector s Float) 
    }

instance VGM.MVector VUM.MVector (L2 VU.Vector Float) where
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
--         forM_ [0..VU.length uv-1] $ \i -> do
            let x = v `VG.unsafeIndex` i
            VGM.unsafeWrite (vecM uv) (start+i) x
        where
            start = loc*elemsizerealM uv


-------------------------------------------------------------------------------
-- immutable vector

data instance VU.Vector (L2 VU.Vector Float) = UnsafeVector 
    { elemsize :: !Int
    , elemsizereal :: !Int
    , len :: !Int
    , vec :: !(L2 VU.Vector Float) 
    }

instance VG.Vector VU.Vector (L2 VU.Vector Float) where
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

-------------------------------------------------------------------------------
-- vecMtor opts

-- data UnsafeVector a = UnsafeVector 
--     { lenM :: !Int
--     , arr :: !ByteArray
--     }
-- 
-- data UnsafeMVector s a = UnsafeMVector
--     { lenMM :: !Int
--     , arrM :: !(MutableByteArray s)
--     }
-- 
-- instance VGM.MVector UnsafeMVector PtFloat where
--     basicLength uv = lenMM uv
--     basicUnsafeSlice = error "UnsafeMVector.basicUnsafeSlice"
--     basicOverlaps uv1 uv2 = error "UnsafeMVector.basicOverlaps" 
--     basicUnsafeNew i = do
--         let lenM = i*getptlenM*sIZEOF_FLOAT
--         arr <- newAlignedPinnedByteArray (2^12) lenM
--         return $ UnsafeMVector
--             { lenMM = lenM
--             , arrM = arr
--             }
--     basicUnsafeRead uv i = 
--         return $ PtFloat $ plusAddr (mutableByteArrayContents $ arrM uv) (i*getptlenM*sIZEOF_FLOAT)
-- 
-- newtype PtFloat = PtFloat { unPtFloat :: Addr } 
-- 
-- 
