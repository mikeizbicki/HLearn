{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds #-}

import System.Environment
import System.IO
import System.CPUTime

import Control.DeepSeq
import Control.Applicative
import Data.Csv
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Console.CmdArgs.Implicit

import Debug.Trace
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG

import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree
import HLearn.Models.Distributions

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
    -- cmd line args
    params <- cmdArgs sample
    let ref = reference_file params
    let query = case query_file params of
            Nothing -> reference_file params
            Just file -> file

    -- reference tree
    Right (xs :: V.Vector DP) <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile $ ref 
    let reftree = parallel train xs :: CoverTree DP
    timeIO "building reference tree" $ return $ rnf reftree
    putStrLn $ "reference tree nodes = " ++ show (stNumNodes reftree)
    putStrLn $ "reference tree max depth = " ++ show (stMaxDepth reftree)
    putStrLn $ "reference tree max children = " ++ show (stMaxChildren reftree)
    putStrLn $ "reference tree ave children = " ++ show (mean $ stAveChildren reftree)
    putStrLn $ "reference tree check --- covering = " ++ show (property_covering reftree)
    putStrLn $ "reference tree check --- leveled = " ++ show (property_leveled reftree)
    putStrLn $ "reference tree check --- separating = " ++ show (property_separating reftree)
    -- renderSVG "reftree.svg" (Width 500) (draw reftree)

    -- query tree
    Right (qs :: V.Vector DP) <- timeIO "loading query dataset" $ fmap (decode False) $ BS.readFile query
    let querytree = parallel train xs :: CoverTree DP
    timeIO "building query tree" $ return $ rnf querytree

    -- knn
    let res=knn2 (DualTree reftree querytree) :: KNN2 1 (Double,Double)
    timeIO "computing knn" $ return $ rnf res
     
    -- end
    putStrLn "end"

timeIO :: String -> IO a -> IO a
timeIO str f = do 
    putStr $ str ++ "... "
    hFlush stdout
    time1 <- getCPUTime
    ret <- f
    seq ret $ return ()
    time2 <- getCPUTime
    putStrLn $ "done. " ++ show ((fromIntegral $ time2-time1)/1e12 :: Double) ++ " seconds"
    return ret
