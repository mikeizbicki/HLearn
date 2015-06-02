module HLearn.Data.UnsafeVector
    ( setptsize
    )
    where

import Control.DeepSeq
import Control.Monad
import Data.Bits
import Data.IORef
import Debug.Trace
import qualified Data.Foldable as F
import Data.Primitive
import qualified Data.Primitive as Prim
import Data.Primitive.MachDeps
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe
import qualified Data.Strict.Maybe as Strict

import Data.Csv

import qualified Prelude as P
import Control.Monad.Primitive
import Foreign.Ptr hiding (sizeOf)
import qualified Foreign.Storable as Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Unsafe.Coerce
-- import Data.Hashable
import Data.Primitive.ByteArray
import GHC.Prim
import GHC.Int
import GHC.Types

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return, liftM, forM_, when)
-- import SubHask.Compatibility.Vector.HistogramMetrics
import SubHask.Compatibility.Vector.Lebesgue

import HLearn.Data.Vector

-------------------------------------------------------------------------------
-- unsafe globals

{-# NOINLINE ptsizeIO #-}
ptsizeIO = unsafeDupablePerformIO $ newIORef (5::Int)

{-# NOINLINE ptalignIO #-}
ptalignIO = unsafeDupablePerformIO $ newIORef (5::Int)

{-# NOINLINE ptsize #-}
ptsize = unsafeDupablePerformIO $ readIORef ptsizeIO

{-# NOINLINE ptalign #-}
ptalign = unsafeDupablePerformIO $ readIORef ptalignIO

-- {-# NOINLINE setptsize #-}
setptsize :: Int -> IO ()
setptsize len = do
    writeIORef ptsizeIO len
    writeIORef ptalignIO (1::Int)

-------------------------------------------------------------------------------
-- UVector2

instance
    ( IsScalar elem
    , ClassicalLogic elem
    , Unbox elem
    , Prim elem
    ) => Unbox (UVector2 (n::Symbol) elem)

---------------------------------------

data instance VU.Vector (UVector2 (n::Symbol) elem) = UArray_UVector
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- ^ offset
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( IsScalar elem
    , Unbox elem
    , Prim elem
    ) => VG.Vector VU.Vector (UVector2 (n::Symbol) elem)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_UVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i len' (UArray_UVector arr off n size) = UArray_UVector arr (off+i*size) len' size

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (UArray_MUVector marr off n size) = do
        arr <- unsafeFreezeByteArray marr
        return $ UArray_UVector arr off n size

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (UArray_UVector arr off n size)= do
        marr <- unsafeThawByteArray arr
        return $ UArray_MUVector marr off n size

--     {-# INLINABLE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (UArray_UVector arr off n size) i =
        return $ UVector2_Dynamic arr (off+i*size) size

--     {-# INLINABLE basicUnsafeCopy #-}
--     basicUnsafeCopy mv v = VG.basicUnsafeCopy (vecM mv) (vec v)

---------------------------------------

data instance VUM.MVector s (UVector2 (n::Symbol) elem) = UArray_MUVector
    {-#UNPACK#-}!(MutableByteArray s)
    {-#UNPACK#-}!Int -- ^ offset in number of elem
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( ClassicalLogic elem
    , IsScalar elem
    , Unbox elem
    , Prim elem
    ) => VGM.MVector VUM.MVector (UVector2 (n::Symbol) elem)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_MUVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' (UArray_MUVector marr off n size) = UArray_MUVector marr (off+i*size) lenM' size

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (UArray_MUVector marr1 off1 n1 size) (UArray_MUVector marr2 off2 n2 _)
        = sameMutableByteArray marr1 marr2

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew lenM' = do
        let elemsize=ptsize
        marr <- newPinnedByteArray (lenM'*elemsize*Prim.sizeOf (undefined::elem))
        return $ UArray_MUVector marr 0 lenM' elemsize

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead mv@(UArray_MUVector marr off n size) i = do
        let b=Prim.sizeOf (undefined::elem)
        marr' <- newPinnedByteArray (size*b)
        copyMutableByteArray marr' 0 marr ((off+i*size)*b) (size*b)
        arr <- unsafeFreezeByteArray marr'
        return $ UVector2_Dynamic arr 0 size

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite mv@(UArray_MUVector marr1 off1 _ size) loc v@(UVector2_Dynamic arr2 off2 _) =
        copyByteArray marr1 ((off1+size*loc)*b) arr2 (off2*b) (size*b)
        where
            b=Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeCopy #-}
    basicUnsafeCopy (UArray_MUVector marr1 off1 n1 size1) (UArray_MUVector marr2 off2 n2 size2) =
        copyMutableByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = size1*Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeMove #-}
    basicUnsafeMove (UArray_MUVector marr1 off1 n1 size1) (UArray_MUVector marr2 off2 n2 size2) =
        moveByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = size1*Prim.sizeOf (undefined::elem)

----------------------------------------
-- Labeled'

instance
    ( Unbox y
    , Prim y
    , ClassicalLogic a
    , IsScalar a
    , Unbox a
    , Prim a
    ) => Unbox (Labeled' (UVector2 (s::Symbol) a) y)

---------------------------------------

-- newtype instance VUM.MVector s (Labeled' x y) = UMV_Labeled' (VUM.MVector s (x,y))

data instance VUM.MVector s (Labeled' (UVector2 (n::Symbol) elem) y) = UArray_Labeled'_MUVector
    {-#UNPACK#-}!(MutableByteArray s)
    {-#UNPACK#-}!Int -- ^ offset in number of elem
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( ClassicalLogic elem
    , IsScalar elem
    , Unbox elem
    , Prim elem
    , Prim y
    ) => VGM.MVector VUM.MVector (Labeled' (UVector2 (n::Symbol) elem) y)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_Labeled'_MUVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' (UArray_Labeled'_MUVector marr off n size)
        = UArray_Labeled'_MUVector marr (off+i*(size+ysize)) lenM' size
        where
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (UArray_Labeled'_MUVector marr1 off1 n1 size) (UArray_Labeled'_MUVector marr2 off2 n2 _)
        = sameMutableByteArray marr1 marr2

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew lenM' = do
        let elemsize=ptsize
        marr <- newPinnedByteArray (lenM'*(elemsize+ysize)*Prim.sizeOf (undefined::elem))
        return $ UArray_Labeled'_MUVector marr 0 lenM' elemsize
        where
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead mv@(UArray_Labeled'_MUVector marr off n size) i = do
        marr' <- newPinnedByteArray (size*b)
        copyMutableByteArray marr' 0 marr ((off+i*(size+ysize))*b) (size*b)
        arr <- unsafeFreezeByteArray marr'
        let x=UVector2_Dynamic arr 0 size
        y <- readByteArray marr $ (off+i*(size+ysize)+size) `div` ysize
        return $ Labeled' x y
        where
            b=Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite
        (UArray_Labeled'_MUVector marr1 off1 _ size)
        i
        (Labeled' (UVector2_Dynamic arr2 off2 _) y)
        = do
            copyByteArray marr1 ((off1+i*(size+ysize))*b) arr2 (off2*b) (size*b)
            writeByteArray marr1 ((off1+i*(size+ysize)+size) `div` ysize) y
        where
            b=Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeCopy #-}
    basicUnsafeCopy
        (UArray_Labeled'_MUVector marr1 off1 n1 size1)
        (UArray_Labeled'_MUVector marr2 off2 n2 size2)
        = copyMutableByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = (size1+ysize)*Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeMove #-}
    basicUnsafeMove
        (UArray_Labeled'_MUVector marr1 off1 n1 size1)
        (UArray_Labeled'_MUVector marr2 off2 n2 size2)
        = moveByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = (size1+ysize)*Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

----------------------------------------

data instance VU.Vector (Labeled' (UVector2 (n::Symbol) elem) y) = UArray_Labeled'_UVector
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- ^ offset
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( IsScalar elem
    , Unbox elem
    , Prim elem
    , Prim y
    ) => VG.Vector VU.Vector (Labeled' (UVector2 (n::Symbol) elem) y)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_Labeled'_UVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i len' (UArray_Labeled'_UVector arr off n size)
        = UArray_Labeled'_UVector arr (off+i*(size+ysize)) len' size
        where
            ysize=Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (UArray_Labeled'_MUVector marr off n size) = do
        arr <- unsafeFreezeByteArray marr
        return $ UArray_Labeled'_UVector arr off n size

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (UArray_Labeled'_UVector arr off n size)= do
        marr <- unsafeThawByteArray arr
        return $ UArray_Labeled'_MUVector marr off n size

--     {-# INLINABLE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (UArray_Labeled'_UVector arr off n size) i =
        return $ Labeled' x y
        where
            off' = off+i*(size+ysize)
            x = UVector2_Dynamic arr off' size
--             y = indexByteArray arr $ (loc+size) `div` ysize
            y = indexByteArray arr $ (off'+size) `shiftR` 1

            ysize=2--Prim.sizeOf (undefined::y) `div` Prim.sizeOf (undefined::elem)

-------------------------------------------------------------------------------
-- l2 vector ops

{-
instance (IsScalar elem, ClassicalLogic elem, Unbox elem) => Unbox (UVector (n::Symbol) elem)

data instance VUM.MVector s (UVector (n::Symbol) elem) = UnsafeMVector
    { elemsizeM     :: {-#UNPACK#-} !Int
    , elemsizerealM :: {-#UNPACK#-} !Int
    , lenM          :: {-#UNPACK#-} !Int
    , vecM          :: !(VUM.MVector s elem)
    }

instance
    ( ClassicalLogic elem
    , IsScalar elem
    , Unbox elem
    ) => VGM.MVector VUM.MVector (UVector (n::Symbol) elem)
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
        liftM UVector_Dynamic $ VG.freeze $ VGM.unsafeSlice (i*elemsizerealM uv) (elemsizeM uv) (vecM uv)

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
--             let x = v `VG.unsafeIndex` i
            let x = v!i
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

data instance VU.Vector (UVector (n::Symbol) elem) = UnsafeVector
    { elemsize     :: {-# UNPACK #-} !Int
    , elemsizereal :: {-# UNPACK #-} !Int
    , len          :: {-# UNPACK #-} !Int
    , vec          :: !(VU.Vector elem)
    }

instance
    ( IsScalar elem
    , Unbox elem
    ) => VG.Vector VU.Vector (UVector (n::Symbol) elem)
        where

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze uv = do
        vec' <- VG.basicUnsafeFreeze (vecM uv)
        return $ UnsafeVector
            { elemsize = elemsizeM uv
            , elemsizereal = elemsizerealM uv
            , len = lenM uv
            , vec = vec'
            }

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw uv = do
        vecM' <- VG.basicUnsafeThaw (vec uv)
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

--     {-# INLINABLE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM uv i = return $ UVector_Dynamic $ VG.basicUnsafeSlice (i*elemsizereal uv) (elemsize uv) (vec uv)

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
--     {-# INLINABLE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (UMV_Labeled' v) = liftM UV_Labeled' $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (UV_Labeled' v) = liftM UMV_Labeled' $ VG.basicUnsafeThaw v
    basicLength (UV_Labeled' v) = VG.basicLength v
    basicUnsafeSlice i len (UV_Labeled' v) = UV_Labeled' $ VG.basicUnsafeSlice i len v
    basicUnsafeIndexM (UV_Labeled' v) i = do
        (!x,!y) <- VG.basicUnsafeIndexM v i
        return $ Labeled' x y
        -}
