{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Random
import qualified Data.Foldable as F
import System.Environment
import System.IO
-- import System.Console.ANSI

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.DataStructures.KDIsomorphism
import HLearn.DataStructures.FourD

main = do
    let testname="results-theory-offset=2^50,range=10000"
    hproc <- openFile (testname++"-proc.csv") WriteMode
    hraw <- openFile (testname++"-raw.csv") WriteMode
    sequence_ [runTest hproc hraw len offset range | len<-[100000], offset<-[2^50], range<-[10000]]
    hClose hproc
    hClose hraw

runTest :: Handle -> Handle -> Int -> Double -> Double -> IO ()
runTest hproc hraw len offset range = do
    hPutStrLn stderr $ "Test "++show len++" "++show offset++" "++show range
    hPutStr stderr "  Training model... "
    m :: KDVector FourD <- fmap train $ randdata len offset range
    hPutStrLn stderr "Done\n"
    xs <- forM [0..25] $ \i -> do
        let dp = FourD (range/2+offset) (range/2+offset) offset offset
        dp:xs <- randdata 1 offset range
--         let x = mindist'' dp m
--         let total = sum $ F.toList $ fmap (\x -> if x then 1 else 0) $ mindist2prunelist x m
        let hylo = prunefoldr (emptyhylo dp) m 
        let total = minvisit (bound hylo) m
        let total_theory = minvisit_theory (bound hylo) m
        hPutStrLn hraw $ show len++", "++show offset++", "++show range++", "++show total++", "++show total_theory++"     "++show (dist hylo)
        hPutStrLn stderr $ "\ESC[1A  test "++show i++"; total prunable = "++ show total++", "++show total_theory++"     "

        return $ (fromIntegral total,fromIntegral total_theory)
--     putStrLn $ "Prunable List = " ++ show xs
--         hFlush hproc
--         hFlush hraw
    let norm = train $ fmap fst xs :: Normal Double Double
    let norm_theory = train $ fmap snd xs :: Normal Double Double
    hPutStrLn stderr $ "  mean = "++show (mean norm)++"; stddev="++show (sqrt $ variance norm)++"  || Theory: mean = "++show (mean norm_theory)++"; stddev="++show (sqrt $ variance norm_theory)
    hPutStrLn hproc $ show len++", "++show offset++", "++show range++", "++show (mean norm)++", "++show (sqrt $ variance norm)++", "++show (mean norm_theory)++", "++show (sqrt $ variance norm_theory)
--     putStrLn $ show len++", "++show offset++", "++show range++", "++show (mean norm)++","++show (variance norm)++",    "++(init $ tail $ show xs)
    hFlush hproc
    hFlush hraw


