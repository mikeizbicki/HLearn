{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts #-}


import Control.DeepSeq
import Control.Applicative
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Proxy
import Data.Reflection
import Data.Time.Clock
import System.Console.CmdArgs.Implicit
import System.Environment
import System.IO
import System.CPUTime
import Numeric

import Test.QuickCheck hiding (verbose,sample)
import Debug.Trace
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG

import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.Models.Distributions

-- type DP = VU.Vector Float
type DP = VU.Vector Double
-- type DP = (Double,Double)
-- type DP = (Double,Double,Double,Double,Double)

instance Num r => HasRing (VU.Vector r) where
    type Ring (VU.Vector r) = r

instance (VU.Unbox r, RealFrac r,Floating r,Show r) => MetricSpace (VU.Vector r) where
--     distance !v1 !v2 = sqrt $ VU.foldl1' (+) $ VU.zipWith (\a b -> (a-b)*(a-b)) v1 v2
--     distance !v1 !v2 = sqrt $ VU.last $ VU.scanl1' (+) $ VU.zipWith (\a b -> (a-b)*(a-b)) v1 v2
    distance !v1 !v2 = sqrt $ go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !v1 !v2 !dist = go 0 (VU.length v1-1) 
        where
            dist2=dist*dist

            go tot (-1) = False 
            go tot i = if tot'>dist2
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)

property_isFartherThan :: VU.Vector Double -> VU.Vector Double -> Double -> Bool
property_isFartherThan dp1 dp2 dist = (distance dp1 dp2 > abs dist) == isFartherThan dp1 dp2 (abs dist)

instance Arbitrary (VU.Vector Double) where
    arbitrary = do
        x1 <- arbitrary
        x2 <- arbitrary
        return $ VU.fromList [x1,x2]

data Params = Params
    { k :: Int
    , reference_file :: Maybe String 
    , query_file :: Maybe String
    , distances_file :: String
    , neighbors_file :: String 
    , verbose :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { k              = 1 &= help "Number of nearest neighbors to find" 
    , reference_file = def &= help "Reference data set in CSV format" &= typFile
    , query_file     = def &= help "Query data set in CSV format" &= typFile &= opt (Nothing :: Maybe String)
    , distances_file = "distances_hlearn.csv" &= help "File to output distances into" &= typFile
    , neighbors_file = "neighbors_hlearn.csv" &= help "File to output the neighbors into" &= typFile
    , verbose        = def &= help "print debugging information" &= typFile 
    }
    &= summary "HLearn k-nearest neighbor, version 1.0"

main = do
    -- cmd line args
    params <- cmdArgs sample

    let checkfail x t = if x then error t else return ()
    checkfail (reference_file params == Nothing) "must specify a reference file"

    -- build reference tree
    let ref = fromJust $ reference_file params
    Right (rs :: V.Vector DP) <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile $ ref 
    let reftree = parallel train rs :: CoverTree DP 
    timeIO "building reference tree" $ return $ rnf reftree

--     renderSVG "reftree.svg" (Width 500) $ draw reftree

    -- build query tree
    querytree <- case query_file params of
        Nothing -> return reftree
        Just file -> do
            Right (qs::V.Vector DP) <- timeIO "loading query dataset" $ fmap (decode False) $ BS.readFile file
            let tmptree=parallel train qs :: CoverTree DP
            timeIO "building query tree" $ return $ deepseq tmptree tmptree

    -- do knn search
--     let action = knn2_single_slow (DualTree reftree querytree) :: KNN2 2 DP
    let action = knn2_parallel (DualTree reftree querytree) :: KNN2 2 DP
--     let action = knn2_slow (DualTree reftree querytree) :: KNN2 2 DP
    res <- timeIO "computing knn_p" $ return $ deepseq action action 

    -- output to files
    let rs_index = Map.fromList $ zip (V.toList rs) [0..]

    timeIO "outputing distance" $ do
        hDistances <- openFile (distances_file params) WriteMode
        sequence_ $ 
            map (hPutStrLn hDistances . concat . intersperse "," . map (\x -> showEFloat (Just 10) x "")) 
            . Map.elems 
            . Map.mapKeys (\k -> fromJust $ Map.lookup k rs_index) 
            . Map.map (map neighborDistance . getknn) 
            $ getknn2 res 
        hClose hDistances

    timeIO "outputing neighbors" $ do
        hNeighbors <- openFile (neighbors_file params) WriteMode
        sequence_ $ 
            map (hPutStrLn hNeighbors . init . tail . show)
            . Map.elems 
            . Map.map (map (\v -> fromJust $ Map.lookup v rs_index)) 
            . Map.mapKeys (\k -> fromJust $ Map.lookup k rs_index) 
            . Map.map (map neighbor . getknn) 
            $ getknn2 res 
        hClose hNeighbors

    -- end
    putStrLn "end"



{-
timeIO "computing nn_fast" $ return $ rnf (nearestNeighbor (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing nn_fast" $ return $ rnf (nearestNeighbor (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing nn_fast" $ return $ rnf (nearestNeighbor (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing nn_slow" $ return $ rnf (nearestNeighbor_slow (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing nn_slow" $ return $ rnf (nearestNeighbor_slow (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing nn_slow" $ return $ rnf (nearestNeighbor_slow (0,0) reftree :: Neighbor(Double,Double))
timeIO "computing knn a" $ return $ rnf (knn_a (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn a" $ return $ rnf (knn_a (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn a" $ return $ rnf (knn_a (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn b" $ return $ rnf (knn_b (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn b" $ return $ rnf (knn_b (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn b" $ return $ rnf (knn_b (0,0) reftree :: KNN 1 (Double,Double))
timeIO "computing knn_s" $ return $ rnf (knn2_single (DualTree reftree querytree) :: KNN2 1 (Double,Double))
timeIO "computing knn_s" $ return $ rnf (knn2_single (DualTree reftree querytree) :: KNN2 1 (Double,Double))
timeIO "computing knn_s" $ return $ rnf (knn2_single (DualTree reftree querytree) :: KNN2 1 (Double,Double))
timeIO "computing knn2" $ return $ rnf (knn2 (DualTree reftree querytree) :: KNN2 1 (Double,Double))
timeIO "computing knn2" $ return $ rnf (knn2 (DualTree reftree querytree) :: KNN2 1 (Double,Double))
timeIO "computing knn2" $ return $ rnf (knn2 (DualTree reftree querytree) :: KNN2 1 (Double,Double))
-}

timeIO :: NFData a => String -> IO a -> IO a
timeIO str f = do 
    putStr $ str ++ replicate (40-length str) '.'
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
