{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators #-}

module Utils
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Csv
import Data.Time.Clock
import Numeric
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro
import System.IO
import System.CPUTime
-- import qualified Data.Vector.Generic.Mutable as VGM
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as VM

import HLearn.Algebra
import HLearn.Models.Distributions

loaddata :: 
    ( VG.Vector v r
    , NFData (v r)
    , FromRecord (v r)
    , Ord r
    , Show r
    , Floating r
    ) => String -> IO (V.Vector (v r))
loaddata filename = do
    rse :: Either String (V.Vector (v r))  
        <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile filename
    rs <- case rse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show (V.length rs)
    putStrLn $ "    num dim: " ++ show (VG.length $ rs V.! 0)
    putStrLn ""

    let shufflemap = mkShuffleMap rs
    putStrLn "  shufflemap:"
--     forM [0..V.length shufflemap-1] $ \i -> do
--         putStrLn $ "    " ++ show (fst $ shufflemap V.! i) ++ ": " ++ show (snd $ shufflemap V.! i) 
    return $ VG.map (shuffleVec $ VG.map fst shufflemap) rs

-- | calculate the variance of each column, then sort so that the highest variance is first
mkShuffleMap :: (VG.Vector v a, Floating a, Ord a) => V.Vector (v a) -> V.Vector (Int,a)
mkShuffleMap v = runST $ do
    let numdim = VG.length (v V.! 0)
    varV :: VM.MVector s (Int, a) <- VGM.new numdim 
    forM [0..numdim-1] $ \i -> do
        let xs   = fmap (VG.! i) v
            dist = train xs :: Normal a a
            var  = variance dist
        VM.write varV i (i,var)
    Intro.sortBy (\(_,v2) (_,v1) -> compare v2 v1) varV
    VG.freeze varV

shuffleVec :: VG.Vector v a => V.Vector Int -> v a -> v a
shuffleVec vmap v = runST $ do
    ret <- VGM.new (VG.length v)
    forM [0..VG.length v-1] $ \i -> do
        VGM.write ret i $ v VG.! (vmap VG.! i)
    VG.freeze ret


timeIO :: NFData a => String -> IO a -> IO a
timeIO str f = do 
    putStr $ str ++ replicate (45-length str) '.'
    hFlush stdout
    cputime1 <- getCPUTime
    realtime1 <- getCurrentTime >>= return . utctDayTime
    ret <- f
    deepseq ret $ return ()
    cputime2 <- getCPUTime
    realtime2 <- getCurrentTime >>= return . utctDayTime
    
    putStrLn $ "done"
        ++ ". real time=" ++ show (realtime2-realtime1) 
        ++ "; cpu time=" ++ showFFloat (Just 6) ((fromIntegral $ cputime2-cputime1)/1e12 :: Double) "" ++ "s"
    return ret

