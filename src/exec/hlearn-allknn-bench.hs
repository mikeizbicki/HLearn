{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad.Random hiding (fromList)
import Data.List (zip,zipWith,intersperse,init,tail,isSuffixOf,sortBy)
import Data.Maybe (fromJust)
import Data.Version
import Numeric
import System.Console.CmdArgs.Implicit
import System.Exit
import System.IO

import qualified Prelude as P
import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Container
import SubHask.Algebra.Ord
import SubHask.Algebra.Parallel
import SubHask.Algebra.Vector
import SubHask.Compatibility.Containers

-- import HLearn.Data.Graph
-- import HLearn.Data.Image
import HLearn.Data.LoadData
import HLearn.Data.SpaceTree
import HLearn.Data.SpaceTree.CoverTree hiding (head,tail)
import HLearn.Data.SpaceTree.Algorithms.NearestNeighbor
import HLearn.History
-- import HLearn.History.Timing
import HLearn.Models.Distributions

import Paths_HLearn

-- import qualified Prelude as P
-- import Control.Concurrent
-- import Control.Parallel
-- import Control.Parallel.Strategies
-- import System.IO.Unsafe
--
-- import Foreign.Ptr

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- command line parameters

data Params = Params
    { k                 :: Int
    , kForceSlow        :: Bool

    , data_format       :: DataFormat
    , reference_file    :: Maybe String
    , query_file        :: Maybe String
    , distances_file    :: String
    , neighbors_file    :: String

    , maxrefdp          :: Maybe Int
    , seed              :: Maybe Int

    , train_method      :: TrainMethod
    , adopt_children    :: Bool
    , train_sequential  :: Bool
    , cache_dists       :: Bool
    , rotate            :: DataRotate

    , searchEpsilon     :: Float
    , expansionRatio    :: Float

    , packMethod        :: PackMethod
    , sortMethod        :: SortMethod

    , verbose           :: Bool
    , debug             :: Bool
    }
    deriving (Show, Data, Typeable)

data TrainMethod
    = TrainInsert_Orig
    | TrainInsert_NoSort
    | TrainInsert_Sort
    | TrainInsert_Parent
    | TrainInsert_Ancestor
    | TrainMonoid
    deriving (Read,Show,Data,Typeable)

data PackMethod
    = NoPack
    | PackCT
    | PackCT2
    | PackCT3
    | SetLeaves
    deriving (Read,Show,Data,Typeable)

data SortMethod
    = NoSort
    | NumDP_Distance
    | NumDP_Distance'
    | Distance_NumDP
    | Distance_NumDP'
    deriving (Read,Show,Data,Typeable)

data DataFormat
    = DF_CSV
    | DF_PLG
    | DF_Images
    | DF_BagOfWords
    | DF_String
    deriving (Read,Show,Data,Typeable)

data DataRotate
    = Variance
    | PCA
    | NoRotate
    deriving (Read,Show,Data,Typeable)

allknnParams = Params
    { k              = 1
                    &= help "Number of nearest neighbors to find"

    , reference_file = def
                    &= help "Reference data set"
                    &= name "r"
                    &= typFile

    , data_format    = DF_CSV
                    &= help "file format of data files"

    , query_file     = def
                    &= help "Query data set"
                    &= name "q"
                    &= typFile

    , distances_file = "distances_hlearn.csv"
                    &= help "File to output distances into"
                    &= name "d"
                    &= typFile

    , neighbors_file = "neighbors_hlearn.csv"
                    &= help "File to output the neighbors into"
                    &= name "n"
                    &= typFile

    , searchEpsilon   = 0
                    &= help ""
                    &= groupname "Approximations"

    , expansionRatio = 1.3
                    &= help ""
                    &= groupname "Approximations"

    , packMethod     = PackCT
                    &= help "Specifies which method to use for cache layout of the covertree"
                    &= groupname "Tree structure optimizations"

    , sortMethod     = NumDP_Distance
                    &= help "What order should the children be sorted in?"

    , kForceSlow     = False
                    &= help "Don't use precompiled k function; use the generic one"

    , train_sequential = False
                    &= help "don't train the tree in parallel; this may *slightly* speed up the nearest neighbor search at the expense of greatly slowing tree construction"

    , train_method   = TrainInsert_NoSort
                    &= help "which method to use to construct the cover tree?"

    , adopt_children = False
                    &= help "move children to uncle nodes when they're closer than parents"

    , cache_dists    = True
                    &= help "pre-calculate the maximum distance from any node dp to all of its descendents; speeds up queries at the expense of O(n log n) overhead in construction"

    , rotate         = NoRotate
                    &= groupname "Data Preprocessing"
                    &= help "Rotate the data points.  May speed up nearest neighbor queries at the expense of longer preprocessing time."

    , verbose        = False
                    &= help "Print tree statistics (takes some extra time)"
                    &= groupname "Debugging"

    , maxrefdp       = Nothing
                    &= help "Take at most this many points from the dataset to build the reference tree"
                    &= groupname "Debugging"

    , seed           = Nothing
                    &= help "If this option is set, the datapoints will be randomly shuffled with the corresponding seed before tree building"
                    &= groupname "Debugging"

    , debug          = False
                    &= help "Test created trees for validity (takes lots of time)"
                    &= name "runtests"
                    &= explicit
    }
    &= summary ("HLearn k-nearest neighbor, version " ++ showVersion version)

-------------------------------------------------------------------------------
-- main

main = do

    -- validate the cmd line args
    params <- cmdArgs allknnParams

    when (reference_file params == Nothing) $
        error "must specify a reference file"

    when (searchEpsilon params < 0) $
        error "search epsilon must be >= 0"

    -- load the data
    let filepath = fromJust $ reference_file params

    let k = Proxy::Proxy 1

    case data_format params of
        DF_CSV -> do
            let ct = Proxy::Proxy (CoverTree_ 2 BArray BArray)
                dp = Proxy::Proxy (Labeled' (UVector "dyn" Float) Int)

            let {-# INLINE loadfile_dfcsv #-} -- prevents space leaks
                loadfile_dfcsv filepath = do
                    rs :: BArray (UVector "dyn" Float) <- loadCSV filepath

                    rs' <- case rotate params of

                        PCA -> error "PCARotate temporarily removed"

                        Variance -> do
                            let shuffleMap = mkShuffleMap rs
                            return $ map (apShuffleMap shuffleMap) $ toList rs

                        NoRotate -> return $ toList rs

                    return $ fromList $ zipWith Labeled' rs' [0::Int ..]

            allknn params loadfile_dfcsv ct dp k

        {-
        DF_Images -> do
            let ct = Proxy::Proxy (CoverTree_ (Static (13/10)) BArray BArray)
                dp = Proxy::Proxy (Labeled' (ColorSig Float) FilePath)

            let {-# INLINE loaddata #-}
                loaddata = loadDirectory
                    (maxrefdp params)
                    (loadColorSig False)
                    (isSuffixOf ".sig.all")
                    true

            allknn params loaddata ct dp k


        DF_PLG -> do
            let ct = Proxy::Proxy (CoverTree_ (Static (13/10)) BArray BArray)
                dp = Proxy::Proxy (Labeled' Graph FilePath)

            let {-# INLINE loaddata #-}
                loaddata = loadDirectory
                    (maxrefdp params)
                    (loadPLG False)
                    isFileTypePLG
                    isNonemptyGraph

            allknn params loaddata ct dp k
            -}

-- | Given our data, perform the actual tests.
-- For efficiency, it is extremely important that this function get specialized to the exact types it is called on.
{-# INLINE allknn #-}
allknn :: forall k exprat childC leafC dp l proxy1 proxy2 proxy3.
    ( ValidCT exprat childC leafC (Labeled' dp l)
    , ValidCT exprat childC leafC dp
    , RationalField (Scalar dp)
    , ValidNeighbor (Labeled' dp l)

    , Index (leafC (Labeled' dp l)) ~ Int
    , Scalar (leafC (Labeled' dp l)) ~ Int
    , IxContainer (leafC (Labeled' dp l))
    , Bounded (Scalar dp)
    , Scalar (childC (CoverTree_ exprat childC leafC (Labeled' dp l))) ~ Int
    , Unboxable (Labeled' dp l)
    , Unboxable dp

    , P.Ord l
    , NFData l
    , Show l

    , Unbox (Labeled' dp l)
    , Unbox dp
    , Unbox l
    ) => Params
      -> (FilePath -> IO (UArray (Labeled' dp l)))
      -> proxy1 (CoverTree_ exprat childC leafC)
      -> proxy2 (Labeled' dp l)
      -> Proxy (1::Nat)
      -> IO ()
allknn params loaddata _ _ _ = runHistory timeDisplay $ do

    -- load the dataset
    rs <- liftIO' $ loaddata $ fromJust $ reference_file params

    let rs_take = case maxrefdp params of
            Nothing -> toList rs
            Just n  -> P.take n $ toList rs

    let rs_shuffle = fromList $ case seed params of
            Nothing -> rs_take
            {-Just n  -> evalRand (shuffle rs_take) (mkStdGen n)-}

    -- build the trees
    reftree :: CoverTree_ exprat childC leafC (Labeled' dp l)
        <- buildTree params rs_shuffle

    (querytree,qs) <- case query_file params of
        Nothing -> liftIO' $ return $ (reftree,rs)
--         Just qfile -> do
--             when (qfile=="/dev/null") $ do
--                 exitSuccess
--
--             qs <- loaddata qfile
--             querytree <- buildTree params qs
--             return (querytree, qs)

    -- do knn search
    let res = unsafeParallelInterleaved
            ( findAllNeighbors (convertRationalField $ searchEpsilon params) reftree  )
            ( stToList querytree )
            :: [(Labeled' dp l, NeighborList 1 (Labeled' dp l))]
--             :: Seq (Labeled' dp l, NeighborList 1 (Labeled' dp l))
    withMsg "computing parFindNeighborMap" res

    -- output to files
    let sortedResults :: [[Neighbor (Labeled' dp l)]]
        sortedResults
            = map (getknnL . snd)
            . sortBy (\(Labeled' _ y1,_) (Labeled' _ y2,_) -> P.compare y1 y2)
            $ toList res
    withMsg "sorting results" sortedResults

    -- output distances
    let distanceL = map (map neighborDistance) sortedResults

    withMsgIO "outputing distance" $ do
        hDistances <- openFile (distances_file params) WriteMode
        forM_ distanceL (hPutStrLn hDistances . init . tail . show)
        hClose hDistances

    -- output neighbors
    let neighborL = (map (map (yLabeled' . neighbor))) sortedResults

    withMsgIO "outputting neighbors" $ do
        hNeighbors <- openFile (neighbors_file params) WriteMode
        forM_ neighborL (hPutStrLn hNeighbors . init . tail . show)
        hClose hNeighbors

    liftIO' $ putStrLn "done"

-- | Gives us many possible ways to construct our cover trees based on the input parameters.
-- This is important so we can compare their runtime features.
buildTree :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    , Unboxable dp
    , Unbox dp
    ) => Params
      -> UArray dp
      -> History Show (CoverTree_ exprat childC leafC dp)
buildTree params xs = do

    liftIO' $ setexpratIORef $ P.toRational $ expansionRatio params

    let trainmethod = case train_method params of
            TrainInsert_Orig     -> trainInsertOrig
            TrainInsert_NoSort   -> trainInsertNoSort
            TrainInsert_Sort     -> trainInsert addChild_nothing
            TrainInsert_Parent   -> trainInsert addChild_parent
            TrainInsert_Ancestor -> trainInsert addChild_ancestor

    let (Just' reftree) = {-parallel-} trainmethod $ toList xs
    withMsg "building tree" reftree

    -- Everything below here uses a sequential algorithm (may change in the future).
    -- These run faster if we disable multithreading.
    do

        let reftree_adopt = if adopt_children params
                then ctAdoptNodes reftree
                else reftree
        withMsg "adopting" reftree_adopt

        let reftree_sort = case sortMethod params of
                NoSort -> reftree_adopt
                NumDP_Distance  -> sortChildren cmp_numdp_distance  reftree_adopt
                NumDP_Distance' -> sortChildren cmp_numdp_distance' reftree_adopt
                Distance_NumDP  -> sortChildren cmp_distance_numdp  reftree_adopt
                Distance_NumDP' -> sortChildren cmp_distance_numdp' reftree_adopt
        withMsg "sorting children" reftree_sort

        let reftree_prune = case packMethod params of
                NoPack    -> reftree_sort
                SetLeaves -> setLeaves 0 $ reftree_sort
                PackCT    -> packCT $ setLeaves 0 $ reftree_sort
        withMsg "packing reference tree" reftree_prune

        let reftree_cache = if cache_dists params
                then setMaxDescendentDistance reftree_prune
                else reftree_prune
        withMsg "caching distances" reftree_cache

        liftIO' $ when (verbose params) $ do
            putStrLn ""
            printTreeStats "reftree      " $ reftree
            printTreeStats "reftree_prune" $ reftree_prune

        return reftree_cache

-- | Print out debugging information about our cover trees.
--
-- FIXME:
-- Make this more generic so it works on any space tree.
-- Should this be moved into the SpaceTree file?
printTreeStats ::
    ( ValidCT exprat childC leafC dp
    ) => String
      -> CoverTree_ exprat childC leafC dp
      -> IO ()
printTreeStats str t = do
    putStrLn (str++" st stats:")
    putStr (str++"  stNumDp..............") >> hFlush stdout >> putStrLn (show $ stNumDp t)
    putStr (str++"  stNumNodes...........") >> hFlush stdout >> putStrLn (show $ stNumNodes t)
    putStr (str++"  stNumLeaves..........") >> hFlush stdout >> putStrLn (show $ stNumLeaves t)
    putStr (str++"  stNumGhosts..........") >> hFlush stdout >> putStrLn (show $ stNumGhosts t)
    putStr (str++"  stNumGhostSingletons.") >> hFlush stdout >> putStrLn (show $ stNumGhostSingletons t)
    putStr (str++"  stNumGhostLeaves.....") >> hFlush stdout >> putStrLn (show $ stNumGhostLeaves t)
    putStr (str++"  stNumGhostSelfparent.") >> hFlush stdout >> putStrLn (show $ stNumGhostSelfparent t)
    putStr (str++"  stAveGhostChildren...") >> hFlush stdout >> putStrLn (show $ mean $ stAveGhostChildren t)
    putStr (str++"  stMaxLeaves..........") >> hFlush stdout >> putStrLn (show $ stMaxLeaves t)
    putStr (str++"  stAveLeaves..........") >> hFlush stdout >> putStrLn (show $ mean $ stAveLeaves t)
    putStr (str++"  stMaxChildren........") >> hFlush stdout >> putStrLn (show $ stMaxChildren t)
    putStr (str++"  stAveChildren........") >> hFlush stdout >> putStrLn (show $ mean $ stAveChildren t)
    putStr (str++"  stMaxDepth...........") >> hFlush stdout >> putStrLn (show $ stMaxDepth t)
    putStr (str++"  stNumSingletons......") >> hFlush stdout >> putStrLn (show $ stNumSingletons t)
    putStr (str++"  stExtraLeaves........") >> hFlush stdout >> putStrLn (show $ stExtraLeaves t)
    putStrLn (str++" ct stats:")
    putStr (str++"  ctMaxCoverRatio........") >> hFlush stdout >> putStrLn (show $ ctMaxCoverRatio t)
    putStr (str++"  ctAveCoverRatio........") >> hFlush stdout >> putStrLn (show $ mean $ ctAveCoverRatio t)
    putStr (str++"  ctMovableNodes.........") >> hFlush stdout >> putStrLn (show $ ctMovableNodes t)
    putStr (str++"  ctBetterMovableNodes...") >> hFlush stdout >> putStrLn (show $ ctBetterMovableNodes t)
    putStr (str++"  ctMovableParents.......") >> hFlush stdout >> putStrLn (show $ ctMovableParents t)
    putStr (str++"  ctBetterMovableParents.") >> hFlush stdout >> putStrLn (show $ ctBetterMovableParents t)

    putStrLn (str++" invariants:")
    putStr (str++"  covering.....") >> hFlush stdout >> putStrLn (show $ invariant_CoverTree_covering t)
    putStr (str++"  tightCover...") >> hFlush stdout >> putStrLn (show $ invariant_CoverTree_tightCovering t)
    putStr (str++"  separating...") >> hFlush stdout >> putStrLn (show $ invariant_CoverTree_separating t)
    putStr (str++"  maxDescDist..") >> hFlush stdout >> putStrLn (show $ invariant_CoverTree_maxDescendentDistance t)
--     putStr (str++"  leveled......") >> hFlush stdout >> putStrLn (show $ property_leveled t)

    putStrLn ""
