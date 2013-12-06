module Data.BitTwiddler
    where

import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import Data.Bits
import Data.Primitive.ByteArray
import GHC.Word
import GHC.TypeLits
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- BitTwiddler

newtype BitTwiddler a = BitTwiddler ByteArray

{-# INLINE numWord64 #-}
numWord64 :: ByteArray -> Int
numWord64 arr = sizeofByteArray arr `intdiv` 8

{-# INLINE intdiv #-}
intdiv :: Int -> Int -> Int
intdiv x y = floor $ (fromIntegral x :: Double) / (fromIntegral y)

instance NFData (BitTwiddler a) where
    rnf (BitTwiddler arr) = seq arr ()
--     rnf (BitTwiddler arr) = runST $ forM_ [0..sizeofByteArray arr-1] $ \i -> do
--         x :: Word8 <- indexByteArrayM arr i
--         rnf x

---------------------------------------
-- comparison

instance Eq (BitTwiddler a) where
    {-# INLINE (==) #-}
    bw1 == bw2 = compare bw1 bw2 == EQ

instance Ord (BitTwiddler a) where
    {-# INLINE compare #-}
    compare (BitTwiddler arr1) (BitTwiddler arr2) = go $ numWord64 arr1 -1 
        where
            go (-1) = EQ
            go i = case compare x1 x2 of
                LT -> LT
                GT -> GT
                EQ -> go (i-1)
                where
                    x1 = indexByteArray arr1 i :: Word64
                    x2 = indexByteArray arr2 i :: Word64

---------------------------------------
-- show

instance Show (BitTwiddler a) where
    show = showBitTwiddler word8bits
    
showBitTwiddler f = \ (BitTwiddler arr) -> concat $ map ((' ':) . f . indexByteArray arr) [0..sizeofByteArray arr-1] 

showBitTwiddlerDouble = \ (BitTwiddler arr) -> concat $ map (word8bits . indexByteArray arr) [0..sizeofByteArray arr-1] 

word8hex :: Word8 -> String
word8hex x = nibbleHex n1 ++ nibbleHex n2
    where
        n1 = x .&. (bit 0 .|. bit 1 .|. bit 2 .|. bit 3) 
        n2 = (shiftL x 4) .&. (bit 0 .|. bit 1 .|. bit 2 .|. bit 3) 

        nibbleHex 0 = "0"
        nibbleHex 1 = "1"
        nibbleHex 2 = "2"
        nibbleHex 3 = "3"
        nibbleHex 4 = "4"
        nibbleHex 5 = "5"
        nibbleHex 6 = "6"
        nibbleHex 7 = "7"
        nibbleHex 8 = "8"
        nibbleHex 9 = "9"
        nibbleHex 10 = "a"
        nibbleHex 11 = "b"
        nibbleHex 12 = "c"
        nibbleHex 13 = "d"
        nibbleHex 14 = "e"
        nibbleHex 15 = "f"

word8bits :: Word8 -> String
word8bits x = concat $ map go [0..7]
    where
        go i = if testBit x i
            then "1"
            else "0"

---------------------------------------
-- read 

string2bitwise :: String -> BitTwiddler a 
string2bitwise xs = BitTwiddler $ runST $ do 
    arr <- newByteArray (length bitList)
    mapM_ (\(i,x) -> writeByteArray arr i x) $ zip [0..] bitList 
    unsafeFreezeByteArray arr
    where
        bitList = go $ filter (/=' ') xs
        go [] = []
        go ys = str2word8 (take 8 ys) : go (drop 8 ys) 

str2word8 :: String -> Word8
str2word8 xs = go xs 0 0
    where
        go :: String -> Int -> Word8 -> Word8
        go ys 8 word8 = word8
        go ys i word8 = go (tail ys) (i+1) $ if head ys == '1'
            then setBit word8 i
            else word8

-------------------------------------------------------------------------------
-- Bits

instance Bits (BitTwiddler a) where
    {-# INLINE (.&.) #-}
    {-# INLINE (.|.) #-}
    {-# INLINE xor #-}
    {-# INLINE complement #-}
    (BitTwiddler arr1) .&. (BitTwiddler arr2) = BitTwiddler $ runST $ do
        let size = sizeofByteArray arr1
        ret <- newByteArray size
        forM_ [0..size-1] $ \i -> do
            let word = indexByteArray arr1 i .&. indexByteArray arr2 i :: Word8
            writeByteArray ret i word
        ret <- unsafeFreezeByteArray ret
        return ret

    (BitTwiddler arr1) .|. (BitTwiddler arr2)  = BitTwiddler $ runST $ do
        let size = sizeofByteArray arr1
        ret <- newByteArray size
        forM_ [0..size-1] $ \i -> do
            let word = indexByteArray arr1 i .|. indexByteArray arr2 i :: Word8
            writeByteArray ret i word
        ret <- unsafeFreezeByteArray ret
        return ret

    complement (BitTwiddler arr) = BitTwiddler $ runST $ do
        let size = sizeofByteArray arr
        ret <- newByteArray size
        forM_ [0..size-1] $ \i -> do
            let word = indexByteArray arr i :: Word8
            writeByteArray ret i $ complement word
        ret <- unsafeFreezeByteArray ret
        return ret

    (BitTwiddler arr1) `xor` (BitTwiddler arr2)  = BitTwiddler $ runST $ do
        let size = sizeofByteArray arr1
        ret <- newByteArray size
        forM_ [0..size-1] $ \i -> do
            let word = indexByteArray arr1 i `xor` indexByteArray arr2 i :: Word8
            writeByteArray ret i word
        ret <- unsafeFreezeByteArray ret
        return ret

    bit = undefined
    testBit = undefined
    bitSize = undefined
    isSigned = undefined
    popCount = undefined

-------------------------------------------------------------------------------
-- merging 

-- | performs a "bitwise zip" on two "BitTwiddler".  They must contain an even number of "Word64"s, and the same amount.
bitzip :: BitTwiddler a -> BitTwiddler b -> BitTwiddler (a,b)
bitzip (BitTwiddler arr1) (BitTwiddler arr2) = BitTwiddler $ runST $ do
    ret <- newByteArray (sizeofByteArray arr1 *2)
    forM [0..floor $ (fromIntegral $ sizeofByteArray arr1 :: Rational) /8-1]  (\i -> do 
        writeByteArray ret (2*i)   $ word1 i
        writeByteArray ret (2*i+1) $ word2 i
        )
    unsafeFreezeByteArray ret
    where
        arr1word i = indexByteArray arr1 i :: Word64
        arr2word i = indexByteArray arr2 i :: Word64

        word1 i = word1arr1 i .|. word1arr2 i
        word1arr1 i = foldr1 (.|.) $ map (go (arr1word i) 0) [0..31] 
        word1arr2 i = foldr1 (.|.) $ map (go (arr2word i) 1) [0..31] 

        word2 i = word2arr1 i .|. word2arr2 i
        word2arr1 i = foldr1 (.|.) $ map (go (arr1word i) (-64)) [32..63] 
        word2arr2 i = foldr1 (.|.) $ map (go (arr2word i) (-63)) [32..63] 

        go :: Word64 -> Int -> Int -> Word64
        go x offset i = if testBit x i
            then bit $ i*2+offset
            else 0

-- | performs a "bitwise unzip" on a "BitTwiddler".  It must contain an even number of "Word64"s.
bitunzip :: BitTwiddler (a,b) -> (BitTwiddler a,BitTwiddler b)
bitunzip (BitTwiddler arr) = runST $ do
    ret1 <- newByteArray (ceiling $ (fromIntegral $ sizeofByteArray arr :: Rational) / 2)
    ret2 <- newByteArray (ceiling $ (fromIntegral $ sizeofByteArray arr :: Rational) / 2)
    forM [0..floor $ (fromIntegral $ sizeofByteArray arr :: Rational) /16-1]  (\i -> do 
        writeByteArray ret1 i $ word1 i
        writeByteArray ret2 i $ word2 i
        )
    ret1 <- unsafeFreezeByteArray ret1
    ret2 <- unsafeFreezeByteArray ret2
    return (BitTwiddler ret1, BitTwiddler ret2)
    where
        arr1word i = indexByteArray arr (i*2)   :: Word64
        arr2word i = indexByteArray arr (i*2+1) :: Word64

        word1 i = word1arr1 i .|. word1arr2 i
        word1arr1 i = foldr1 (.|.) $ map (go (arr1word i) 0 0) [0..31]
        word1arr2 i = foldr1 (.|.) $ map (go (arr2word i) 0 32) [0..31]

        word2 i = word2arr1 i .|. word2arr2 i
        word2arr1 i = foldr1 (.|.) $ map (go (arr1word i) 1 0) [0..31]
        word2arr2 i = foldr1 (.|.) $ map (go (arr2word i) 1 32) [0..31]

        go :: Word64 -> Int -> Int -> Int-> Word64
        go x arrnum offset i = if testBit x (i*2+arrnum)
            then bit $ i+offset
            else 0
--
--
-------------------------------------------------------------------------------
-- ToBitTwiddler

class ToBitTwiddler a where
    toBitTwiddler :: a -> BitTwiddler a
    fromBitTwiddler :: BitTwiddler a -> a

instance ToBitTwiddler Word64 where
    {-# INLINE toBitTwiddler#-}
    {-# INLINE fromBitTwiddler#-}
    toBitTwiddler x = BitTwiddler $ runST $ do
        arr <- newByteArray 8
        writeByteArray arr 0 x
        unsafeFreezeByteArray arr
    fromBitTwiddler (BitTwiddler arr) = indexByteArray arr 0

instance ToBitTwiddler Double where
    {-# INLINE toBitTwiddler#-}
    {-# INLINE fromBitTwiddler#-}
    toBitTwiddler x = BitTwiddler $ runST $ do
        arr <- newByteArray 8
        if x>=0
            then writeByteArray arr 0 $ complementBit (unsafeCoerce x :: Word64) 63
            else writeByteArray arr 0 $ complement (unsafeCoerce x :: Word64)
        unsafeFreezeByteArray arr
    fromBitTwiddler (BitTwiddler arr) 
        | x == maxBound = 0
        | testBit x 63 = unsafeCoerce $ complementBit x 63
        | otherwise = unsafeCoerce $ complement x
        where
            x = indexByteArray arr 0 :: Word64

-- -- instance (ToBitTwiddler a) => ToBitTwiddler [a] where
-- instance ToBitTwiddler [Double] where
--     toBitTwiddler [x] = toBitTwiddler x
--     toBitTwiddler xs = bitzip (toBitTwiddler odds) (toBitTwiddler evens)
--         where
--             odds  = map snd $ filter (odd  . fst) $ zip [0..] xs
--             evens = map snd $ filter (even . fst) $ zip [0..] xs
-- 
--     fromBitTwiddler (BitTwiddler arr) = if sizeofByteArray arr<=8
--         then [fromBitTwiddler (BitTwiddler arr)]
--         else weave (fromBitTwiddler b) (fromBitTwiddler a)
--         where
--             (a,b) = bitunzip (BitTwiddler arr)
-- 
--             weave [] [] = []
-- --             weave [] ys = ys
-- --             weave xs [] = xs
--             weave (x:xs) (y:ys) = x:y:(weave xs ys)
-- 

-------------------------------------------------------------------------------
-- QuickCheck
        
property_wellordered :: Double -> Double -> Bool
property_wellordered x y = -- trace ("\n\nx="++show x++"\ny="++show y++"\nx'="++show x'++"\ny'="++show y'++" -- "++show (compare x y)++" -- "++show (compare x' y')) $ 
    compare x y == compare x' y'
    where
        x' = toBitTwiddler x
        y' = toBitTwiddler y

property_wellordered1 :: Word64 -> Word64 -> Bool
property_wellordered1 x y = property_wellordered (unsafeCoerce x) (unsafeCoerce y)

property_tofrominverse :: Double -> Bool
property_tofrominverse x = x == fromBitTwiddler (toBitTwiddler x)

-------------------------------------------------------------------------------
-- tests

x=toBitTwiddler (maxBound :: Word64)
y=toBitTwiddler (minBound :: Word64)
xx = bitzip x x
xy = bitzip x y
yx = bitzip y x
yy = bitzip y y
xxxx = bitzip xx xx

twiddle :: BitTwiddler Double
twiddle = BitTwiddler $ runST $ do
    arr <- newByteArray 8
    unsafeFreezeByteArray arr

