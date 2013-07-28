module HLearn.DataStructures.KDIsomorphism
    where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Primitive.ByteArray
import GHC.Word
import Unsafe.Coerce

import Test.QuickCheck hiding ((.&.),(.|.))

-------------------------------------------------------------------------------
-- Bitwise

newtype Bitwise = Bitwise ByteArray

instance Eq Bitwise where
    bw1 == bw2 = compare bw1 bw2 == EQ

instance Ord Bitwise where
    compare (Bitwise arr1) (Bitwise arr2) = go 0
        where
            go i = case compare x1 x2 of
                LT -> LT
                GT -> GT
                EQ -> if i < sizeofByteArray arr1
                    then go (i+1)
                    else EQ
                where
                    x1 = indexByteArray arr1 i :: Word64
                    x2 = indexByteArray arr2 i :: Word64

---------------------------------------
-- show

instance Show Bitwise where
    show = showBitwise word8bits
    
showBitwise f = \ (Bitwise arr) -> concat $ map ((' ':) . f . indexByteArray arr) [0..sizeofByteArray arr-1] 

showBitwiseDouble = \ (Bitwise arr) -> concat $ map (word8bits . indexByteArray arr) [0..sizeofByteArray arr-1] 

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

string2bitwise :: String -> Bitwise
string2bitwise xs = Bitwise $ runST $ do 
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
-- ToBitwise

class (Ord a) => ToBitwise a where
    toBitwise :: a -> Bitwise
    fromBitwise :: Bitwise -> a

instance ToBitwise Word64 where
    toBitwise x = Bitwise $ runST $ do
        arr <- newByteArray 8
        writeByteArray arr 0 x
        unsafeFreezeByteArray arr

instance ToBitwise Double where
    toBitwise x = Bitwise $ runST $ do
        arr <- newByteArray 8
        if x>0
            then writeByteArray arr 0 $ complementBit (unsafeCoerce x :: Word64) 63
            else writeByteArray arr 0 $ complement (unsafeCoerce x :: Word64)
        unsafeFreezeByteArray arr
    fromBitwise (Bitwise arr) 
        | x == maxBound = 0
        | testBit x 63 = unsafeCoerce $ complementBit x 63
        | otherwise = unsafeCoerce $ complement x
        where
            x = indexByteArray arr 0 :: Word64

-------------------------------------------------------------------------------
-- QuickCheck
        
property_wellordered :: Double -> Double -> Bool
property_wellordered x y = if x > y
    then x' > y'
    else x' < y'
    where
        x' = toBitwise x
        y' = toBitwise y

property_tofrominverse :: Double -> Bool
property_tofrominverse x = x == fromBitwise (toBitwise x)
