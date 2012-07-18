{-# LANGUAGE BangPatterns #-}

module MiscUtils
    where

import Base

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import Debug.Trace

        
thaw2 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw2 vv = join $ liftM V.thaw $ vSequence $ V.map V.thaw vv

thaw3 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw3 vv = do
    newvec <- VM.unsafeNew $ V.length vv
    sequence 
        [ do
            v <- V.thaw $ vv V.! i
            VM.write newvec i v
        | i <- [0..(V.length vv)-1]
        ]
    return newvec
        
vSequence :: Monad m => V.Vector (m a) -> m (V.Vector a)
vSequence v = liftM V.fromList $ sequence $ V.toList v

foldMTrace :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldMTrace f x ys = foldMTraceItr 0 (length ys) f x ys
    where
        foldMTraceItr !itr !len f !x [] = return $! x
        foldMTraceItr !itr !len f !x (y:ys) = do
--             fm <- f x y
            fm <- if {-True -- -}(itr `mod` 10==0)
                     then trace ("itr="++show itr++"/"++show len) $ f x y
                     else let z = f x y in z `seq` z
            foldMTraceItr (itr+1) len f fm ys

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

normalizeL :: [Probability] -> [Probability]
normalizeL xs = {-trace ("xs==0"++show (map (==0) xs)) $ 
                trace ("sum==0"++show (s==0)) $ 
                trace ("normxs="++show (map (/s) xs)) $ 
                trace ("max normxs="++show (maximum $ map (/s) xs)) $ 
                -}map (/s) xs
    where
        s = sum xs
