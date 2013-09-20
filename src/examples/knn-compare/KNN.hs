{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable #-}

import System.Environment
import System.IO
import System.CPUTime

import Control.DeepSeq
import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Console.CmdArgs.Implicit

import HLearn.Algebra
import HLearn.DataStructures.KDTree hiding (mindist)
-- import HLearn.DataStructures.KDIsomorphism --hiding (mindist)
-- import HLearn.DataStructures.SortedVector
import HLearn.DataStructures.CoverTree

-- instance FromRecord (a,a) where
--     parseRecord v = FourD <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3

type DP = (Double,Double)

data Params = Params
    { reference_file :: String 
    , k :: Int
    , distances_file :: String
    , neighbors_file :: String 
    , query_file :: Maybe String
    , verbose :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { reference_file = def &= help "File containing the reference data set in CSV format" &= typFile
    , k = def &= help "Number of nearest neighbors to find" 
    , distances_file = def &= help "File to output distances into" &= typFile
    , neighbors_file = def &= help "File to output the neighbors into" &= typFile
    , query_file = def &= help "query data set in CSV format" &= typFile &= opt (Nothing :: Maybe String)
    , verbose = def &= help "print debugging information" &= typFile 
    }
    &= summary "HLearn k-nearest neighbor, version 1.0"

main = do
    params <- cmdArgs sample
    let ref = reference_file params
    let query = case query_file params of
            Nothing -> reference_file params
            Just file -> file
    Right (xs :: V.Vector DP) <- timeIO "Loading dataset" $ fmap (decode False) $ BS.readFile $ ref 
    Right (qs :: V.Vector DP) <- timeIO "Loading query" $ fmap (decode False) $ BS.readFile query
    let kdtree = parallel train xs :: CoverTree DP
    timeIO "Building kd-tree" $ return $ rnf kdtree
--     dp <- timeIO "KNN" $ return $ mindist (qs V.! 0) kdtree
--     print dp
--     (dist,dp) <- timeIO "KNN" $ return $ mindist (qs V.! 0) kdtree
--     print (dist,dp)
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
