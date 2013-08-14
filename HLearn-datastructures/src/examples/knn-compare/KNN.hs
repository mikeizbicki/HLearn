{-# LANGUAGE ScopedTypeVariables,TemplateHaskell #-}

import System.Environment
import System.IO
import System.CPUTime

import Control.DeepSeq
import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS

import HLearn.Algebra
import HLearn.DataStructures.KDTree
import HLearn.DataStructures.KDIsomorphism hiding (mindist)
import HLearn.DataStructures.SortedVector

instance FromRecord FourD where
    parseRecord v = FourD <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3

-- derivingUnbox "FourD"
--     [t| FourD -> (Double,Double,Double,Double) |]
--     [| \ FourD a b c d -> (a,b,c,d) |]
--     [| \ (a,b,c,d) -> FourD a b c d |]


main = do
    Right (xs :: V.Vector FourD) <- timeIO "Loading dataset" $ fmap (decode False) $ BS.readFile "dataset.csv"
    Right (qs :: V.Vector FourD) <- timeIO "Loading query" $ fmap (decode False) $ BS.readFile "query.csv"
    let kdtree = train xs :: KDVector FourD
    timeIO "Building kd-tree" $ return $ rnf $ kdtree
    (dist,dp) <- timeIO "KNN" $ return $ mindist'' (qs V.! 0) kdtree
    print (dist,dp)
    putStrLn "end"
 

timeIO :: String -> (IO a) -> (IO a)
timeIO str f = do 
    putStr $ str ++ "... "
    hFlush stdout
    time1 <- getCPUTime
    ret <- f
    seq ret $ return ()
--     Right (xs :: V.Vector FourD) <- fmap (decode False) $ BS.readFile "dataset.csv"
    time2 <- getCPUTime
    putStrLn $ "done. " ++ show ((fromIntegral $ time2-time1)/1e12 :: Double) ++ " seconds"
    return ret
