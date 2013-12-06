{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators #-}


import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector.Algorithms.Intro as Intro
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
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.Metrics.Lebesgue
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.Normal
import HLearn.Models.Distributions

-- type DP = L2 V.Vector Double
type DP = L2 VU.Vector Float 
-- type Tree = AddUnit (CoverTree' (2/1)) () DP
-- type Tree = AddUnit (CoverTree' (3/2)) () DP
type Tree = AddUnit (CoverTree' (5/4)) () DP
-- type Tree = AddUnit (CoverTree' (9/8)) () DP
-- type Tree = AddUnit (CoverTree' (17/16)) () DP

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
        1 -> runit params (undefined :: Tree) (undefined :: KNN2 1 DP)
        2 -> runit params (undefined :: Tree) (undefined :: KNN2 2 DP)
        3 -> runit params (undefined :: Tree) (undefined :: KNN2 3 DP)
--         4 -> runit params (undefined :: Tree) (undefined :: KNN2 4 DP)
--         5 -> runit params (undefined :: Tree) (undefined :: KNN2 5 DP)
--         6 -> runit params (undefined :: Tree) (undefined :: KNN2 6 DP)
--         7 -> runit params (undefined :: Tree) (undefined :: KNN2 7 DP)
--         8 -> runit params (undefined :: Tree) (undefined :: KNN2 8 DP)
--         9 -> runit params (undefined :: Tree) (undefined :: KNN2 9 DP)
--         10 -> runit params (undefined :: Tree) (undefined :: KNN2 10 DP)
        otherwise -> error "specified k value not supported"

{-# SPECIALIZE runit :: Params -> Tree -> KNN2 1 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> Tree -> KNN2 2 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> Tree -> KNN2 3 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 4 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 5 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 6 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 7 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 8 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 9 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> KNN2 10 DP -> IO ()#-}
runit :: forall k tree base dp ring. 
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
    , dp ~ DP
    ) => Params -> AddUnit (CoverTree' base) () dp -> KNN2 k dp -> IO ()
runit params tree knn = do

    -- build reference tree
    let ref = fromJust $ reference_file params
    rs <- loaddata ref 

    let reftree = parallel train rs :: CoverTree DP 
    timeIO "building reference tree" $ return reftree
    let reftree_prune = {-setLeafSize 1 $-} unUnit reftree
    timeIO "pruning reference tree" $ return reftree_prune

    -- verbose prints tree stats
    if verbose params 
        then do
            putStrLn ""
            printTreeStats "reftree      " reftree 
            printTreeStats "reftree_prune" $ UnitLift reftree_prune
        else return ()

    
    -- build query tree
    querytree <- case query_file params of
        Nothing -> return $ UnitLift reftree_prune
        Just file -> do
            Right (qs::V.Vector dp) <- timeIO "loading query dataset" $ fmap (decode False) $ BS.readFile file
            undefined

    -- do knn search
    let action = knn2_single_parallel (DualTree (reftree_prune) (reftree_prune)) :: KNN2 k DP
    res <- timeIO "computing knn2_single_parallel" $ return action 

    -- output to files
    let rs_index = Map.fromList $ zip (V.toList rs) [0..]

    timeIO "outputing distance" $ do
        hDistances <- openFile (distances_file params) WriteMode
        sequence_ $ 
            map (hPutStrLn hDistances . concat . intersperse "," . map (\x -> showEFloat (Just 10) x "")) 
            . Map.elems 
            . Map.mapKeys (\k -> fromJust $ Map.lookup k rs_index) 
            . Map.map (map neighborDistance . Strict.strictlist2list . getknn) 
            $ getknn2 res 
        hClose hDistances

    timeIO "outputing neighbors" $ do
        hNeighbors <- openFile (neighbors_file params) WriteMode
        sequence_ $ 
            map (hPutStrLn hNeighbors . init . tail . show)
            . Map.elems 
            . Map.map (map (\v -> fromJust $ Map.lookup v rs_index)) 
            . Map.mapKeys (\k -> fromJust $ Map.lookup k rs_index) 
            . Map.map (map neighbor . Strict.strictlist2list . getknn) 
            $ getknn2 res 
        hClose hNeighbors
    
    -- end
    putStrLn "end"


loaddata :: String -> IO (V.Vector DP)
loaddata filename = do
    rse :: Either String (V.Vector DP)  
        <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile filename
    rs <- case rse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show (V.length rs)
    putStrLn $ "    num dim: " ++ show (VG.length $ rs V.! 0)
    putStrLn ""

    let shufflemap = mkShuffleMap rs
--     putStrLn "  shufflemap:"
--     forM [0..VU.length shufflemap-1] $ \i -> do
--         putStrLn $ "    " ++ show (fst $ shufflemap VU.! i) ++ ": " ++ show (snd $ shufflemap VU.! i) 
    return $ V.map (shuffleVec $ VU.map fst shufflemap) rs

-- | calculate the variance of each column, then sort so that the highest variance is first
mkShuffleMap :: (VG.Vector v a, Floating a, Ord a, VU.Unbox a) => V.Vector (v a) -> VU.Vector (Int,a)
mkShuffleMap v = runST $ do
    let numdim = VG.length (v V.! 0)
    varV :: VUM.MVector s (Int, a) <- VGM.new numdim 
    forM [0..numdim-1] $ \i -> do
        let xs   = fmap (VG.! i) v
            dist = train xs :: Normal a a
            var  = variance dist
        VGM.write varV i (i,var)
    Intro.sortBy (\(_,v2) (_,v1) -> compare v2 v1) varV
    VG.freeze varV

shuffleVec :: VG.Vector v a => VU.Vector Int -> v a -> v a
shuffleVec vmap v = runST $ do
    ret <- VGM.new (VG.length v)
    forM [0..VG.length v-1] $ \i -> do
        VGM.write ret i $ v VG.! (vmap VG.! i)
    VG.freeze ret

-- printTreeStats :: String -> Tree -> IO ()
printTreeStats str t = do
    putStrLn (str++" stats:")
    putStr (str++"  stNumDp..............") >> hFlush stdout >> putStrLn (show $ stNumDp t) 
    putStr (str++"  stNumNodes...........") >> hFlush stdout >> putStrLn (show $ stNumNodes t) 
    putStr (str++"  stNumLeaves..........") >> hFlush stdout >> putStrLn (show $ stNumLeaves t) 
    putStr (str++"  stNumGhosts..........") >> hFlush stdout >> putStrLn (show $ stNumGhosts t) 
    putStr (str++"  stNumGhostSingletons.") >> hFlush stdout >> putStrLn (show $ stNumGhostSingletons t) 
    putStr (str++"  stNumGhostLeaves.....") >> hFlush stdout >> putStrLn (show $ stNumGhostLeaves t) 
    putStr (str++"  stNumGhostSelfparent.") >> hFlush stdout >> putStrLn (show $ stNumGhostSelfparent t) 
    putStr (str++"  stAveGhostChildren...") >> hFlush stdout >> putStrLn (show $ mean $ stAveGhostChildren t) 
    putStr (str++"  stMaxChildren........") >> hFlush stdout >> putStrLn (show $ stMaxChildren t) 
    putStr (str++"  stAveChildren........") >> hFlush stdout >> putStrLn (show $ mean $ stAveChildren t) 
    putStr (str++"  stMaxDepth...........") >> hFlush stdout >> putStrLn (show $ stMaxDepth t) 
    putStr (str++"  stNumSingletons......") >> hFlush stdout >> putStrLn (show $ stNumSingletons t) 
    putStr (str++"  stExtraLeaves........") >> hFlush stdout >> putStrLn (show $ stExtraLeaves t) 

    putStrLn (str++" properties:")
    putStr (str++"  covering...............") >> hFlush stdout >> putStrLn (show $ property_covering t) 
    putStr (str++"  leveled................") >> hFlush stdout >> putStrLn (show $ property_leveled t) 
    putStr (str++"  separating.............") >> hFlush stdout >> putStrLn (show $ property_separating t)
    putStr (str++"  maxDescendentDistance..") >> hFlush stdout >> putStrLn (show $ property_maxDescendentDistance t) 

    putStrLn ""

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
