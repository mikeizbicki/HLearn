{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HLearn.DataStructures.BitTwiddlerSort.BitTwiddler
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import Control.Monad.Primitive
import Data.Bits
import Data.Monoid

import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import Unsafe.Coerce

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Debug.Trace

-------------------------------------------------------------------------------
-- generic functions

{-# INLINE sortM_ #-}
sortM_ :: (PrimMonad m, Storable a) => (Ptr a -> CInt -> IO ()) -> VSM.MVector s a -> m ()
sortM_ c_sort v = unsafePrimToPrim $ do
    let (foreignptr,len) = VSM.unsafeToForeignPtr0 v
    let ptr = unsafeForeignPtrToPtr foreignptr
    c_sort ptr $ fromIntegral len

{-# INLINE sort_ #-}
sort_ :: (Storable a) => (Ptr a -> CInt -> IO ()) -> VS.Vector a -> VS.Vector a
sort_ c_sort v = runST $ do
    v <- VS.thaw v
    sortM_ c_sort v
    v <- VS.freeze v
    return v

class Sortable a where
    timSortM :: (PrimMonad m) => (VSM.MVector s a) -> m ()
    timSort :: VS.Vector a -> VS.Vector a

    quickSortM :: (PrimMonad m) => (VSM.MVector s a) -> m ()
    quickSort :: VS.Vector a -> VS.Vector a

class BitZip a where
    type BitZipType a
    zero :: a
    bitzip :: BitZipType a -> a
    bitunzip :: a -> BitZipType a

class ToBitTwiddler a where
    type BitTwiddler a
    toBitTwiddler :: a -> BitTwiddler a
    fromBitTwiddler :: BitTwiddler a -> a

class BitOrder a where
    bitorderMarshall :: a -> Word64
    bitorderUnmarshall :: Word64 -> a

instance BitOrder Double where
    bitorderMarshall x = if x >= 0
        then complementBit (unsafeCoerce x) 63
        else complement (unsafeCoerce x)
    bitorderUnmarshall x = if testBit x 63
        then unsafeCoerce $ complementBit x 63
        else unsafeCoerce $ complement x

-------------------------------------------------------------------------------
-- BitTwiddler4

data BitTwiddler4 = BitTwiddler4
    { a :: {-# UNPACK #-} !Word64
    , b :: {-# UNPACK #-} !Word64
    , c :: {-# UNPACK #-} !Word64
    , d :: {-# UNPACK #-} !Word64
    }
    deriving Eq

instance NFData BitTwiddler4 where
    rnf a = seq a ()

instance Show BitTwiddler4 where
    show bt = go (a bt) 0 ++ wordsep
           ++ go (b bt) 0 ++ wordsep
           ++ go (c bt) 0 ++ wordsep
           ++ go (d bt) 0 ++ wordsep ++ wordsep
        where
            wordsep = "\n"

            go x 64 = ""
            go x i = bytesep i $ (bit x i):(go x (i+1))
            
            bytesep i = if i`mod`8==0 && i>0
                then (' ':)
                else id

            bit x i = if testBit x i
                then '1'
                else '0'

instance Ord BitTwiddler4 where
    compare bt1 bt2 = compare (d bt1) (d bt2)
                   <> compare (c bt1) (c bt2)
                   <> compare (b bt1) (b bt2)
                   <> compare (a bt1) (a bt2)

instance BitZip BitTwiddler4 where
    type BitZipType BitTwiddler4 = (Word64,Word64,Word64,Word64)
    
    {-# INLINE zero #-}
    {-# INLINABLE bitzip #-}
    {-# INLINABLE bitunzip #-}

    zero = BitTwiddler4 0 0 0 0

--     bitzip (a,b,c,d) = BitTwiddler4 a b c d
--     bitunzip (BitTwiddler4 a b c d) = (a,b,c,d)

    bitzip (a,b,c,d) = go a zero 0 0
                    .|.go b zero 0 1
                    .|.go c zero 0 2
                    .|.go d zero 0 3
        where
            go x bt 64 offset = bt
            go x bt index offset = go x bt' (index+1) offset
                where
                    bt' = if testBit x index 
                        then bt .|. bit (index*4+offset)
                        else bt

    bitunzip bt = (go 0 0 0, go 0 0 1, go 0 0 2, go 0 0 3)  
        where
            go x 64 offset = x
            go x index offset = go x' (index+1) offset
                where
                    x' = if testBit bt (index*4+offset)
                        then x .|. bit index
                        else x

instance Bits BitTwiddler4 where
    {-# INLINE (.&.) #-}
    {-# INLINE (.|.) #-}
    {-# INLINE xor #-}
    {-# INLINE complement #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}
    {-# INLINE isSigned #-}
    {-# INLINE popCount #-}
    bt1 .&. bt2 = BitTwiddler4
        { a = a bt1 .&. a bt2
        , b = b bt1 .&. b bt2
        , c = c bt1 .&. c bt2
        , d = d bt1 .&. d bt2
        }
    bt1 .|. bt2 = BitTwiddler4
        { a = a bt1 .|. a bt2
        , b = b bt1 .|. b bt2
        , c = c bt1 .|. c bt2
        , d = d bt1 .|. d bt2
        }
    bt1 `xor` bt2 = BitTwiddler4
        { a = a bt1 `xor` a bt2
        , b = b bt1 `xor` b bt2
        , c = c bt1 `xor` c bt2
        , d = d bt1 `xor` d bt2
        }
    complement bt = BitTwiddler4
        { a = complement $ a bt
        , b = complement $ b bt
        , c = complement $ c bt
        , d = complement $ d bt
        }

    bit i
        | 0   <= i && i < 64  = BitTwiddler4 (bit i) 0 0 0
        | 64  <= i && i < 128 = BitTwiddler4 0 (bit (i-64)) 0 0
        | 128 <= i && i < 192 = BitTwiddler4 0 0 (bit (i-128)) 0
        | 192 <= i && i < 256 = BitTwiddler4 0 0 0 (bit (i-192))

    testBit x i
        | 0   <= i && i < 64  = testBit (a x) i 
        | 64  <= i && i < 128 = testBit (b x) (i-64)
        | 128 <= i && i < 192 = testBit (c x) (i-128)
        | 192 <= i && i < 256 = testBit (d x) (i-192)

    bitSize _ = 64*4
    isSigned _ = False
    popCount bt = popCount (a bt) + popCount (b bt) + popCount (c bt) + popCount (d bt)

instance Storable BitTwiddler4 where
    {-# INLINE sizeOf #-}
    {-# INLINE alignment #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}

    sizeOf _ = 32
    alignment _ = 32 

    peek addr = do
        a :: Word64 <- peek $ addr `plusPtr` 0
        b :: Word64 <- peek $ addr `plusPtr` 8
        c :: Word64 <- peek $ addr `plusPtr` 16
        d :: Word64 <- peek $ addr `plusPtr` 24
        return $ BitTwiddler4 a b c d 

    poke addr bt = do
        poke (addr `plusPtr` 0) (a bt)
        poke (addr `plusPtr` 8) (b bt)
        poke (addr `plusPtr` 16) (c bt)
        poke (addr `plusPtr` 24) (d bt)

#define wordlen 4
foreign import ccall "BitTwiddlerSort.h BitTwiddler4_tim_sort" 
    c_BitTwiddler4_tim_sort :: Ptr BitTwiddler4 -> CInt -> IO ()
foreign import ccall "BitTwiddlerSort.h BitTwiddler4_quick_sort" 
    c_BitTwiddler4_quick_sort :: Ptr BitTwiddler4 -> CInt -> IO ()

instance Sortable BitTwiddler4 where
    timSortM = sortM_ c_BitTwiddler4_tim_sort
    timSort = sort_ c_BitTwiddler4_tim_sort
    
    quickSortM = sortM_ c_BitTwiddler4_quick_sort
    quickSort = sort_ c_BitTwiddler4_quick_sort
    
-------------------------------------------------------------------------------
-- tests

randBT :: IO (BitTwiddler4)
randBT = do
    a <- randomRIO (1,4)
    b <- randomRIO (1,4)
    c <- randomRIO (1,4)
    d <- randomRIO (1,4)
    return $ BitTwiddler4 a b c d

bt :: Word64 -> BitTwiddler4
bt a = BitTwiddler4 a a a a

testBitTwiddler4 = do
    xs <- replicateM 100 randBT
    let v = quickSort $ VS.fromList xs
    mapM (putStrLn. show) $ VS.toList v
    putStrLn "Done."
