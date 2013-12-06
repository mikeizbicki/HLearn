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

main = do

    let seed = 1

    -- initialize data
    let numdim = 2
        numdatapoints = 1000
        dimL = [10]
        numdpL = [100000]

    let datapoint = do
            dp <- replicateM numdim $ getRandomR (-5,5)
            return $ L2 $ VG.fromList dp

    let query = evalRand datapoint (mkStdGen $ seed-1)

    let dataset1 = evalRand (replicateM numdatapoints datapoint) (mkStdGen seed)
        dataset2 = evalRand (replicateM numdatapoints datapoint) (mkStdGen $ seed+1)
        dataset1' d n = take n dataset1
        dataset2' d n = take n dataset2

    let ct1 = train dataset1 :: CoverTree DP
        ct2 = train dataset2 :: CoverTree DP
        ct1' d n = train $ dataset1' d n :: CoverTree DP
        ct2' d n = train $ dataset2' d n :: CoverTree DP

--     deepseq ct1 $ return ()
--     deepseq ct2 $ return ()

    -- run benchmarks
    let critConfig = defaultConfig 
            { cfgPerformGC   = ljust True
            , cfgSamples     = ljust 1
            , cfgReport      = ljust "report.html"
            , cfgSummaryFile = ljust "covertree-rawsummary.csv"
            }

    let bench1 name f arg = bench2 name (\d n -> f) arg 
        bench2 name f arg = bgroup name 
            [ bench ("_"++show dim++"_"++show num) $ nf (f dim num) (arg dim num) 
            | num <- numdpL
            , dim <- dimL
            ]

    defaultMainWith critConfig (return ())
        [ bgroup "train"
            [ bench1 "hom1" (train :: [DP] -> CoverTree DP) dataset1'
--             , bench1 "hom2" (parallelN 2 train :: [DP] -> CoverTree DP) dataset1'
--             , bench1 "hom3" (parallelN 3 train :: [DP] -> CoverTree DP) dataset1'
--             , bench1 "hom4" (parallelN 4 train :: [DP] -> CoverTree DP) dataset1'
            , bench1 "insert" (trainct_insert :: [DP] -> CoverTree DP) dataset1'
            ]
--         , bgroup "merge"
--             [ bench2 "balanced" (\d n -> (<>) (ct1' d n)) ct2'
--             , bench1 "unbalanced" (ct1 <>) ct2'
--             ]
--         , bgroup "map"
--             [ bgroup "id"
--                 [ bench1 "unsafe" (unsafeMap id) ct1'
--                 , bench1 "safe" (ctmap id) ct1'
--                 ]
--             , bgroup "translate"
--                 [ bench1 "unsafe" (unsafeMap translate) ct1'
--                 , bench1 "safe" (ctmap translate) ct1'
--                 ]
--             , bgroup "shrink"
--                 [ bench1 "unsafe" (unsafeMap shrink) ct1'
--                 , bench1 "safe" (ctmap shrink) ct1'
--                 ]
--             , bgroup "stretch"
--                 [ bench1 "unsafe" (unsafeMap stretch) ct1'
--                 , bench1 "safe" (ctmap stretch) ct1'
--                 ]
--             ]
        ]

translate dp = VG.map (+10) dp
shrink dp = VG.map (/2) dp
stretch dp = VG.map (*2) dp
