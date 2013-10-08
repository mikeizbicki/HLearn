{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving #-}


import Control.DeepSeq
import Control.Applicative
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
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

import Control.Parallel.Strategies
import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.Metrics.Lebesgue
import HLearn.Models.Distributions

type DP = L2 (VU.Vector Double)

instance Num r => HasRing ((VU.Vector r)) where
    type Ring ((VU.Vector r)) = r

instance MetricSpace ((VU.Vector Double)) where
--     distance !v1 !v2 = sqrt $ VU.foldl1' (+) $ VU.zipWith (\a b -> (a-b)*(a-b)) v1 v2
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(v1) !(v2) = {-# SCC distance #-} sqrt $ go 0 (VU.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

    isFartherThan !(v1) !(v2) !dist = {-# SCC isFartherThan #-} go 0 (VU.length v1-1) 
        where
            dist2=dist*dist

            go tot (-1) = False 
            go tot i = if tot'>dist2
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                              *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
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
    , debug :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { k              = 1 &= help "Number of nearest neighbors to find" 
    , reference_file = def &= help "Reference data set in CSV format" &= typFile
    , query_file     = def &= help "Query data set in CSV format" &= typFile &= opt (Nothing :: Maybe String)
    , distances_file = "distances_hlearn.csv" &= help "File to output distances into" &= typFile
    , neighbors_file = "neighbors_hlearn.csv" &= help "File to output the neighbors into" &= typFile
    , verbose        = False &= help "print tree statistics (takes some extra time)" &= typFile 
    , debug          = False &= help "test created trees for validity (takes lots of time)" &= typFile 
    }
    &= summary "HLearn k-nearest neighbor, version 1.0"


main = do
    -- cmd line args
    params <- cmdArgs sample

    let checkfail x t = if x then error t else return ()
    checkfail (reference_file params == Nothing) "must specify a reference file"

    case k params of 
        1 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 1 DP)
        2 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 2 DP)
        3 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 3 DP)
        4 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 4 DP)
        5 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 5 DP)
        6 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 6 DP)
        7 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 7 DP)
        8 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 8 DP)
        9 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 9 DP)
        10 -> runit params (undefined :: CoverTree DP) (undefined :: KNN2 10 DP)
        otherwise -> error "specified k value not supported"

{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 1 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 2 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 3 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 4 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 5 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 6 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 7 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 8 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 9 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> CoverTree DP -> KNN2 10 DP -> IO ()#-}
runit :: forall k tree dp ring. 
    ( MetricSpace dp
    , Ord dp
    , SingI k
    , Show dp
    , Show (Ring dp)
    , NFData dp
    , NFData (Ring dp)
    , RealFloat (Ring dp)
    , FromRecord dp 
    , VU.Unbox (Ring dp)
    ) => Params -> CoverTree dp -> KNN2 k dp -> IO ()
runit params tree knn = do
    -- build reference tree
    let ref = fromJust $ reference_file params
    Right (rs :: V.Vector dp) <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile $ ref 
    let reftree = parallel train rs :: CoverTree dp -- CoverTree DP 
    timeIO "building reference tree" $ return $ rnf reftree

    -- verbose prints tree stats
    if verbose params 
        then do
            putStr "reftree stNumNodes....." >> hFlush stdout >> putStrLn (show $ stNumNodes reftree) 
            putStr "reftree stMaxChildren.." >> hFlush stdout >> putStrLn (show $ stMaxChildren reftree) 
            putStr "reftree stAveChildren.." >> hFlush stdout >> putStrLn (show $ mean $ stAveChildren reftree) 
            putStr "reftree stMaxDepth....." >> hFlush stdout >> putStrLn (show $ stMaxDepth reftree) 
        else return ()

    if debug params
        then do 
            putStr "reftree covering......." >> hFlush stdout >> putStrLn (show $ property_covering reftree) 
            putStr "reftree leveled........" >> hFlush stdout >> putStrLn (show $ property_leveled reftree) 
            putStr "reftree separating....." >> hFlush stdout >> putStrLn (show $ property_separating reftree) 
            --renderSVG "reftree.svg" (Width 500) $ draw reftree
        else return () 

    -- build query tree
    querytree <- case query_file params of
        Nothing -> return reftree
        Just file -> do
            Right (qs::V.Vector dp) <- timeIO "loading query dataset" $ fmap (decode False) $ BS.readFile file
--             let xs = map train $ CK.partition 4 qs :: [CoverTree dp]
--             timeIO "building query tree" $ do
--                 deepseq xs $ return ()
--                 return $ reduce xs
            let tmptree=reduce $ parMap rdeepseq train $ CK.partition 4 qs :: CoverTree dp -- CoverTree DP
            let tmptree=parallel train qs :: CoverTree dp -- CoverTree DP
            timeIO "building query tree" $ return $ deepseq tmptree tmptree

    -- do knn search
    let action = dknn (DualTree reftree querytree) :: KNN2 k dp
--     let action = knn2_single_parallel (DualTree reftree querytree) :: KNN2 k dp
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
