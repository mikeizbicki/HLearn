-- |
--
-- FIXME: incorporate this into the History monad
module HLearn.History.Timing
    where

import SubHask (NFData, deepseq)

import Prelude
import Data.Time.Clock
import Numeric
import System.CPUTime
import System.IO

time :: NFData a => String -> a -> IO a
time str a = timeIO str $ deepseq a $ return a

timeIO :: NFData a => String -> IO a -> IO a
timeIO str f = do
    hPutStr stderr $ str ++ replicate (45-length str) '.'
    hFlush stderr
    cputime1 <- getCPUTime
    realtime1 <- getCurrentTime >>= return . utctDayTime
    ret <- f
    deepseq ret $ return ()
    cputime2 <- getCPUTime
    realtime2 <- getCurrentTime >>= return . utctDayTime

    hPutStrLn stderr $ "done"
        ++ ". real time=" ++ show (realtime2-realtime1)
        ++ "; cpu time=" ++ showFFloat (Just 6) ((fromIntegral $ cputime2-cputime1)/1e12 :: Double) "" ++ "s"
    return ret

