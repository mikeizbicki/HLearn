module Main
    where

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

instance NFData a => NFData (V.Vector a) where
  rnf v = V.foldl' (\x y -> y `deepseq` x) () v

vm_accum :: (Control.Monad.Primitive.PrimMonad m) =>
            (a -> b -> a) -> VM.MVector (PrimState m) a -> [(Int, b)] -> m (VM.MVector (PrimState m) a)
vm_accum f vec [] = return vec
vm_accum f vec ((i,v):xs) = do
    oldV <- VM.read vec i
    VM.write vec i $ f oldV v
    vm_accum f vec xs

-- test :: 
len=100000000

testST = runST $ do
    newvec <- VGM.new len
    VM.set newvec 0
    vm_accum (+) newvec [(i,1) | i<-[0..len-1]]
    V.unsafeFreeze newvec

test = v1
    where 
        v0 = V.replicate len (0::Int)
        v1 = V.accum (+) v0 [(i,1) | i<-[0..len-1]]
        
main = do
    deepseq test $ putStrLn "Done."