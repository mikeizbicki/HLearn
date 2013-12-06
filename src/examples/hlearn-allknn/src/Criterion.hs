{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Safe
import Foreign.Marshal.Utils
import Foreign.Storable
import System.Random
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Criterion.Config
import Criterion.Main
import qualified Data.Strict as Strict

import HLearn.Algebra
import HLearn.Metrics.Lebesgue
import HLearn.DataStructures.SpaceTree.Simple
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    
myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 100
    }
    
-- dp1 = VU.fromList [0,1,2,3,4,5::Float]
-- dp2 = VU.fromList [1,2,2,3,5,3::Float]

dp1 = VU.fromList [938.9631020880993,-233.25671801133808,-648.1827167669653,-228.29292507706248,-32.75766512157111,-851.3582790230391,-630.3710709651775,621.0358456363426,-830.6039289197575,-278.27300240428724,900.1007471115579,522.7556088089011,-423.36901546705303,-447.6307801685573,-921.3020269478691,352.49080369569515,108.63423421860193,227.2457251161893,393.20881883216884,-230.37943386957727 :: Float]

dp2 = VU.fromList [253.63016824481815,12.635772076712328,792.1728155747198,444.175100630162,-191.01848935888574,-300.6034639102227,-133.81795250857851,-84.277832883173,-356.77272051576267,926.1080947107837,-569.5068954204559,-984.932561515461,-485.2108919989828,866.0891839562528,-600.6717567986852,-330.22072306055566,-272.5317925901056,351.8343308265196,1.6537325992532033,859.423945530244 :: Float]

distancetests = 
    [ bench "L2" $ nf (distance (L2 dp1)) (L2 dp2)
    , bench "L2'" $ nf (distance (SquaredL2 dp1)) (SquaredL2 dp2)
    , bench "L1" $ nf (distance (L1 dp1)) (L1 dp2)
    , bench "Linf" $ nf (distance (Linf dp1)) (Linf dp2)
    ]

floattests x y =
    [ bench "id" $ nf id x
    , bench "+" $ nf (+x) y
    , bench "-" $ nf (\y -> x-y) y
    , bench "*" $ nf (*x) y
    , bench "/" $ nf (/x) y
    , bench "sqrt" $ nf sqrt x
    ]


numdim = 20

-- type DP = L2 VS.Vector Double
type DP = (Double,Double)

datapoint = do
--     dp <- replicateM numdim $ getRandomR (-5,5)
--     return $ L2 $ VG.fromList dp
    x1 <- getRandomR (-5,5)
    x2 <- getRandomR (-5,5)
    return (x1,x2)    

-- instance VG.Vector v Double => Storable (L2 v Double) where
instance Storable (L2 VS.Vector Double) where
    sizeOf _ = numdim*8
    alignment _ = 8


    peek ptr = do
        fptr <- newForeignPtr_ $ castPtr ptr
        return $ L2 $ VS.unsafeFromForeignPtr0 fptr numdim
--     peek ptr = do
--         xs <- forM [0..numdim-1] $ \i -> peekElemOff (castPtr ptr) i
--         return $ VG.fromList xs

    poke ptr (L2 v) = do
        return ()
--         let (vfptr,off,len) = VS.unsafeToForeignPtr v
--         let vptr = unsafeForeignPtrToPtr vfptr
--         copyBytes ptr (castPtr vptr) (len*8)
--     poke ptr v = do
--         forM_ [0..numdim-1] $ \i -> pokeElemOff (castPtr ptr) i (v VG.! i)

instance Storable (Double,Double) where
    sizeOf _ = 8*2
    alignment _ = 8
    peek ptr = do
        x1 <- peekElemOff (castPtr ptr) 0
        x2 <- peekElemOff (castPtr ptr) 1
        return $ (x1,x2)
    poke ptr (x1,x2) = do
        pokeElemOff (castPtr ptr) 0 x1
        pokeElemOff (castPtr ptr) 1 x2

main = do
    let seed = 0
    let xs = evalRand (replicateM 100 datapoint) (mkStdGen seed)
    let query = evalRand datapoint (mkStdGen $ seed+1)

    let simple = train xs :: Simple V.Vector DP
    let simpleS = train xs :: Simple VS.Vector DP
    let simpleU = train xs :: Simple VU.Vector DP
    let covertree = train xs :: CoverTree DP

    deepseq simple $ return ()
    deepseq simpleS $ return ()
    deepseq simpleU $ return ()
    deepseq covertree $ return ()

    defaultMainWith myConfig (return ()) $ 
        [ bench "Simple" $ nf (simple_knn query (mempty :: KNN 1 DP)) simple
        , bench "SimpleS" $ nf (simple_knn query (mempty :: KNN 1 DP)) simpleS
        , bench "SimpleU" $ nf (simple_knn query (mempty :: KNN 1 DP)) simpleU
        , bench "CoverTree" $ nf (knn query :: CoverTree DP -> KNN 1 DP) covertree
        ]

--     defaultMainWith myConfig (return ()) distancetests
-- main = defaultMainWith myConfig (return ()) $ (floattests (1.1::Float) (48.78::Float)) ++ floattests (1.1::Double) (48.78::Double)
