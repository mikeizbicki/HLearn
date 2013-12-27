{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Data.List
import System.Random
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.Primitive.ByteArray 
import Data.Primitive.Array 

import Criterion.Config
import Criterion.Main
import qualified Data.Strict as Strict

import HLearn.Algebra
import HLearn.Metrics.Lebesgue
import HLearn.DataStructures.SpaceTree.Simple
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    

arrlen = 20

list2ByteArray xs = runST $ do
    arr <- newAlignedPinnedByteArray (2^16) (arrlen*4)
    forM (zip [0..] xs) $ \(i,x) -> do
        writeByteArray arr i x
    unsafeFreezeByteArray arr

list2Array xs = runST $ do
    arr <- newArray arrlen 0
    forM (zip [0..] xs) $ \(i,x) -> do
        writeArray arr i x
    unsafeFreezeArray arr

instance NFData n => NFData (Array n) where
    rnf a = runST $ forM_ [0..arrlen-1] $ \i -> do
        return $ rnf $ a `indexArray` i

distance_Array_Float :: Array Float -> Array Float -> Float
distance_Array_Float !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexArray` i)-(a2 `indexArray` i))
                          *((a1 `indexArray` i)-(a2 `indexArray` i))) (i-1)

distance_ByteArray_Float :: ByteArray -> ByteArray -> Float
distance_ByteArray_Float !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))) (i-1)

distance_Vector_Float :: V.Vector Float -> V.Vector Float -> Float
distance_Vector_Float !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                          *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

distance_UVector_Float :: VU.Vector Float -> VU.Vector Float -> Float
distance_UVector_Float !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                          *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

distance_UVector_Float_arrlen :: VU.Vector Float -> VU.Vector Float -> Float
distance_UVector_Float_arrlen !v1 !v2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                          *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

distance2_UVector_Float :: VU.Vector Float -> VU.Vector Float -> Float
distance2_UVector_Float !v1 !v2 = go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                          *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

distance_UVector_Float2 :: VU.Vector Float -> VU.Vector Float -> Float
distance_UVector_Float2 !v1 !v2 = sqrt $ go 0 0
    where
        go tot i = if i==VG.length v1
            then tot
            else go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i+1)


critConfig = defaultConfig 
        { cfgPerformGC   = ljust True
        , cfgSamples     = ljust 1000
        }

main = do
    
    let dimL1 :: [Float] = evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen $ 3)
        dimL2 :: [Float] = evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen $ 4)

    let v1 = V.fromList dimL1
        v2 = V.fromList dimL2

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    let a1 = list2Array dimL1
        a2 = list2Array dimL2

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    deepseq v1 $ deepseq v2 $ return ()
    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq a1 $ deepseq a2 $ return ()
    seq ba1 $ seq ba2 $ return ()

    defaultMainWith critConfig (return ())
        [ bgroup "distance"
--             [ bench "V.Vector"  $ nf (distance_Vector_Float v1) v2
            [ bench "VU.Vector" $ nf (distance_UVector_Float vu1) vu2
--             , bench "squared VU.Vector" $ nf (distance2_UVector_Float vu1) vu2
--             , bench "VU.Vector2" $ nf (distance_UVector_Float2 vu1) vu2
--             , bench "Array"     $ nf (distance_Array_Float a1) a2
            , bench "VU.VectorBA" $ nf (distance_UVector_Float_arrlen vu1) vu2
            , bench "ByteArray" $ nf (distance_ByteArray_Float ba1) ba2
            ]
        ]

