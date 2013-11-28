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
import System.Random
import qualified Data.Foldable as F
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
    
type DP = L2 VU.Vector Double

numdim = 20
numdatapoints = 10000

datapoint = do
    dp <- replicateM numdim $ getRandomR (-5,5)
    return $ L2 $ VG.fromList dp

main = do

    let seed = 0

    -- initialize data
    let query = evalRand datapoint (mkStdGen $ seed-1)

    let dataset1 = evalRand (replicateM numdatapoints datapoint) (mkStdGen seed)
        dataset2 = evalRand (replicateM numdatapoints datapoint) (mkStdGen $ seed+1)

    let ct1 = train dataset1 :: CoverTree DP
        ct2 = train dataset2 :: CoverTree DP

    deepseq ct1 $ return ()
    deepseq ct2 $ return ()

    -- run benchmarks
    let critConfig = defaultConfig 
            { cfgPerformGC = ljust True
            , cfgSamples = ljust 3
            }

    defaultMainWith critConfig (return ())
        [ bgroup "train"
            [ bench "hom" $ nf (train :: F.Foldable container => container DP -> CoverTree DP) dataset1
            , bench "insert" $ nf (trainct_insert :: [DP] -> CoverTree DP) dataset1
            ]
        , bgroup "merge"
            [ bench "unsafe" $ nf (unsafeMerge ct1) ct2
            , bench "safe" $ nf (ct1 <>) ct2
            ]
        , bgroup "map"
            [ bgroup "id"
                [ bench "unsafe" $ nf (unsafeMap id) ct1
                , bench "safe" $ nf (ctmap id) ct1
                ]
            , bgroup "translate"
                [ bench "unsafe" $ nf (unsafeMap translate) ct1
                , bench "safe" $ nf (ctmap translate) ct1
                ]
            , bgroup "shrink"
                [ bench "unsafe" $ nf (unsafeMap shrink) ct1
                , bench "safe" $ nf (ctmap shrink) ct1
                ]
            , bgroup "stretch"
                [ bench "unsafe" $ nf (unsafeMap stretch) ct1
                , bench "safe" $ nf (ctmap stretch) ct1
                ]
            ]
        ]

translate dp = VG.map (+10) dp
shrink dp = VG.map (/2) dp
stretch dp = VG.map (*2) dp
