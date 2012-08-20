{-# LANGUAGE BangPatterns #-}

module HMine.MiscUtils
    ( normalizeL
    , Stream (..)
    , lazyDecodeFile
    , vSequence
    , vm_accum
    , module DebugFolds
    )
    where

-- import Base

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Binary
import Data.Binary.Get
import Data.Int
import Data.List
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import System.IO.Unsafe

import DebugFolds
        
thaw2 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw2 vv = join $ liftM V.thaw $ vSequence $ V.map V.thaw vv

thaw3 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw3 vv = do
    newvec <- VM.unsafeNew $ V.length vv
    sequence_ 
        [ do
            v <- V.thaw $ vv V.! i
            VM.write newvec i v
        | i <- [0..(V.length vv)-1]
        ]
    return newvec
        
vSequence :: Monad m => V.Vector (m a) -> m (V.Vector a)
vSequence v = liftM V.fromList $ sequence $ V.toList v

vm_accum :: (Control.Monad.Primitive.PrimMonad m, NFData a) =>
            (a -> b -> a) -> VM.MVector (PrimState m) a -> [(Int, b)] -> m (VM.MVector (PrimState m) a)
vm_accum f vec [] = return vec
vm_accum f vec ((i,v):xs) = do
    oldV <- VM.read vec i
    let newval = f oldV v
    deepseq newval $ VM.write vec i newval
    vm_accum f vec xs

    
instance NFData a => NFData (V.Vector a) where
    rnf v = V.foldl' (\x y -> y `deepseq` x) () v
    
-- instance NFData a => NFData (VM.MVector s a) where
--     rnf v = runST $ do
-- --         VGM.transform (\x -> x) v
--         return ()

-- instance NFData a => NFData (VM.MVector s a) where
--     rnf (VM.MVector i n arr) = unsafeInlineST $ force i
--         where
--           force !ix | ix < n    = do x <- PA.readArray arr ix
--                                      rnf x `seq` force (ix+1)
--                     | otherwise = return ()

normalizeL :: (Fractional a) => [a] -> [a]
normalizeL xs = {-trace ("xs==0"++show (map (==0) xs)) $ 
                trace ("sum==0"++show (s==0)) $ 
                trace ("normxs="++show (map (/s) xs)) $ 
                trace ("max normxs="++show (maximum $ map (/s) xs)) $ 
                -}map (/s) xs
    where
        s = sum xs

-------------------------------------------------------------------------------
-- lazy binary streaming to/from list

-- taken from http://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell
newtype Stream a = Stream { unstream :: [a] }
    deriving (Show,Read)

instance Binary a => Binary (Stream a) where
    put (Stream [])     = putWord8 0
    put (Stream (x:xs)) = putWord8 1 >> put x >> put (Stream xs)

    get = do
        t <- getWord8
        case t of
            0 -> return (Stream [])
            1 -> do x         <- get
                    Stream xs <- get
                    return (Stream (x:xs))

-- lazy decoding
-- taken from http://stackoverflow.com/questions/11695373/lazy-decoding-of-a-list-with-data-binary

getMaybe :: Binary a => Get (Maybe a)
getMaybe = do
    t <- getWord8
    case t of
      0 -> return Nothing
      _ -> fmap Just get
      
step :: Binary a => (BSL.ByteString,Int64) -> Maybe (a,(BSL.ByteString,Int64))
step (xs,offset) = case runGetState getMaybe xs offset of
                     (Just v, ys, newOffset) -> Just (v,(ys,newOffset))
                     _                       -> Nothing

tmp  = encode $ Stream [1 .. 10 :: Integer]
tmp' = encode $ Stream [1 .. 100000000 :: Integer]

lazyDecodeList :: Binary a => BSL.ByteString -> [a]
lazyDecodeList xs = unfoldr step ({-BSL.take 101 -}xs,0)

lazyDecodeStream :: Binary a => BSL.ByteString -> Stream a
lazyDecodeStream = Stream . lazyDecodeList

decodeStream :: BSL.ByteString -> [Integer]
decodeStream bsl = case BSL.head bsl of
                        '\0' -> []
                        '\1' -> ret
    where
        ret = case (BSL.readInteger $ BSL.tail bsl) of
                 Nothing -> error "decodeStream barf"
                 Just (x,bsl') -> trace (show x) $ x:(decodeStream bsl')

lazyDecodeFile :: (Binary a) => FilePath -> IO [a]
lazyDecodeFile file = liftM lazyDecodeList $ BSL.readFile file
