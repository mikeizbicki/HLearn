import Control.Monad
import qualified Data.Foldable as F
import System.Environment
import System.IO
-- import System.Console.ANSI

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.DataStructures.KDIsomorphism

main = do
    let testname="results-varlen-offset=20000,range=10000"
    hproc <- openFile (testname++"-proc.csv") WriteMode
    hraw <- openFile (testname++"-raw.csv") WriteMode
    sequence_ [runTest hproc hraw len offset range | len<-[i*100|i<-[1..100]], offset<-[20000], range<-[10000]]
    hClose hproc
    hClose hraw

runTest :: Handle -> Handle -> Int -> Double -> Double -> IO ()
runTest hproc hraw len offset range = do
    hPutStrLn stderr $ "Test "++show len++" "++show offset++" "++show range
    hPutStr stderr "  Training model... "
    m <- fmap train $ randdata len offset range
    hPutStrLn stderr "Done\n"
    xs <- forM [0..9] $ \i -> do
        let dp = FourD (range/2+offset) (range/2+offset) offset offset
        dp:xs <- randdata 1 offset range
        let x = mindist'' dp m
        let total = sum $ F.toList $ fmap (\x -> if x then 1 else 0) $ mindist2prunelist x m
        hPutStrLn hraw $ show len++", "++show offset++", "++show range++", "++show total
        hPutStrLn stderr $ "\ESC[1A  test "++show i++"; total prunable = "++ show total
        return total
--     putStrLn $ "Prunable List = " ++ show xs
    let norm = train xs :: Normal Double Double
    hPutStrLn stderr $ "  mean = "++show (mean norm)++"; stddev="++show (sqrt $ variance norm)
    hPutStrLn hproc $ show len++", "++show offset++", "++show range++", "++show (mean norm)++", "++show (sqrt $ variance norm)
--     putStrLn $ show len++", "++show offset++", "++show range++", "++show (mean norm)++","++show (variance norm)++",    "++(init $ tail $ show xs)
    hFlush hproc
    hFlush hraw
