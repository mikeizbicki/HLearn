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
import GHC.Exts (inline)
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
import SubHask.Algebra.Vector.FFI
import SubHask.Compatibility.Containers

-- import HLearn.Data.Graph
-- import HLearn.Data.Image
import HLearn.Data.LoadData
import HLearn.Data.SpaceTree
-- import HLearn.Data.SpaceTree.CoverTree
import HLearn.Data.SpaceTree.CoverTree_Specialized
import HLearn.Data.SpaceTree.CoverTree.Unsafe
-- import HLearn.Data.SpaceTree.Algorithms
import HLearn.Data.SpaceTree.Algorithms_Specialized
import HLearn.History.Timing
import HLearn.Models.Distributions

import Paths_HLearn

-------------------------------------------------------------------------------
-- command line parameters

data Params = Params
    { data_format       :: DataFormat
    , reference_file    :: Maybe String
    , query_file        :: Maybe String
    , distances_file    :: String
    , neighbors_file    :: String

    , k                 :: Int
    , kForceSlow        :: Bool


    , maxrefdp          :: Maybe Int
    , seed              :: Maybe Int

    , treetype          :: TreeType
    , arrtype           :: ArrType
    , foldtype          :: FoldType
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

data TreeType
    = Orig
    | Simplified
    | Sort
    | Parent
    | Ancestor
    | TrainMonoid
    deriving (Read,Show,Data,Typeable)

data ArrType
    = Boxed
    | Unboxed
    | List
    deriving (Read,Show,Data,Typeable)

data FoldType
    = FoldSort
    | Fold
    deriving (Show, Data, Typeable)

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
                    &= groupname "Not currently implemented"

    , searchEpsilon  = 1
                    &= help "Instead of finding points with the closest distance, settle for finding points  whose distance is within searchepsilon*closest"
                    &= groupname "Normal use"

    , reference_file = def
                    &= groupname "Normal use"
                    &= help "Reference data set"
                    &= name "r"
                    &= typFile

    , data_format    = DF_CSV
                    &= groupname "Normal use"
                    &= help "file format of data files"

    , query_file     = def
                    &= groupname "Normal use"
                    &= help "Query data set"
                    &= name "q"
                    &= typFile

    , distances_file = "distances_hlearn.csv"
                    &= groupname "Normal use"
                    &= help "File to output distances into"
                    &= name "d"
                    &= typFile

    , neighbors_file = "neighbors_hlearn.csv"
                    &= groupname "Normal use"
                    &= help "File to output the neighbors into"
                    &= name "n"
                    &= typFile

    , expansionRatio = 1.3
                    &= groupname "Optimizations"
                    &= help ""

    , packMethod     = PackCT
                    &= groupname "Optimizations"
                    &= help "Specifies which method to use for cache layout of the covertree"
                    &= groupname "Optimizations"

    , sortMethod     = NumDP_Distance
                    &= groupname "Optimizations"
                    &= help "What order should the children be sorted in?"

    , kForceSlow     = False
                    &= groupname "Optimizations"
                    &= help "Don't use precompiled k function; use the generic one"

    , train_sequential = False
                    &= groupname "Optimizations"
                    &= help "don't train the tree in parallel; this may *slightly* speed up the nearest neighbor search at the expense of greatly slowing tree construction"

    , treetype       = Ancestor
                    &= groupname "Optimizations"
                    &= help "which method to use to construct the cover tree?"

    , arrtype        = Unboxed
                    &= groupname "Optimizations"
                    &= help "boxed or unboxed arrays?"

    , foldtype       = FoldSort
                    &= groupname "Optimizations"
                    &= help "should we sort the data points before folding over them?"

    , adopt_children = False
                    &= groupname "Optimizations"
                    &= help "move children to uncle nodes when they're closer than parents"

    , cache_dists    = True
                    &= groupname "Optimizations"
                    &= help "pre-calculate the maximum distance from any node dp to all of its descendents; speeds up queries at the expense of O(n log n) overhead in construction"

    , rotate         = Variance
                    &= groupname "Optimizations"
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

    when (searchEpsilon params < 1) $
        error "search epsilon must be >= 1"

    -- load the data
    let filepath = fromJust $ reference_file params

    let k = Proxy::Proxy 1

    case data_format params of
        DF_CSV -> do
            let {-# INLINE loadfile_dfcsv #-} -- prevents space leaks
                loadfile_dfcsv filepath = do
                    rs :: BArray (UVector "dyn" Float) <- loadCSV filepath

                    rs' <- case rotate params of

                        PCA -> error "PCARotate temporarily removed"

                        Variance -> disableMultithreading $ do
                            let shuffleMap = mkShuffleMap rs
                            time "mkShuffleMap" shuffleMap
                            time "varshifting data" $ map (apShuffleMap shuffleMap) $ toList rs

                        NoRotate -> return $ toList rs

                    return $ fromList $ zipWith Labeled' rs' [0::Int ..]

            let dp = Proxy::Proxy (Labeled' (UVector "dyn" Float) Int)

            case arrtype params of
                Unboxed -> do
                    let ct = Proxy::Proxy UCoverTree
                    allknn params loadfile_dfcsv ct dp k

--                 Boxed -> do
--                     let ct = Proxy::Proxy BCoverTree
--                     allknn params loadfile_dfcsv ct dp k
--
--                 List -> do
--                     let ct = Proxy::Proxy (CoverTree_ () [] UArray)
--                     allknn params loadfile_dfcsv ct dp k

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
    , RationalField (Scalar dp)
    , Bounded (Scalar dp)
    , Unboxable (Labeled' dp l)

    , Ord l
    , NFData l
    , NFData (childC (CoverTree_ exprat childC leafC (Labeled' dp l)))
    , Show l
    ) => Params
      -> (FilePath -> IO (UArray (Labeled' dp l)))
      -> proxy1 (CoverTree_ exprat childC leafC)
      -> proxy2 (Labeled' dp l)
      -> Proxy (1::Nat)
      -> IO ()
allknn params loaddata _ _ _ = do

    -- load the dataset
    rs <- loaddata $ fromJust $ reference_file params

    let rs_take = case maxrefdp params of
            Nothing -> toList rs
            Just n  -> P.take n $ toList rs

    let rs_shuffle = fromList $ case seed params of
            Nothing -> rs_take
--             Just n  -> evalRand (shuffle rs_take) (mkStdGen n)

    -- build the trees
    reftree :: CoverTree_ exprat childC leafC (Labeled' dp l)
        <- buildTree params rs_shuffle

    (querytree,qs) <- case query_file params of
        Nothing -> return $ (reftree,rs)
        Just qfile -> do
            qs <- loaddata qfile
            querytree <- buildTree params qs
            return (querytree, qs)

    -- do knn search
    let method = case foldtype params of
            FoldSort -> findNeighbor
            Fold     -> findNeighbor_NoSort

    let epsilon = convertRationalField $ searchEpsilon params

--     let res = unsafeParallelInterleaved
    let res = parallelInterleaved
            ( fromList . map (\dp -> (dp, method epsilon reftree dp) ) )
            ( stToList querytree )
            :: ParList (Labeled' dp l, Neighbor (Labeled' dp l))
    time "computing parFindNeighborMap" res
    -- FIXME:
    -- The ParList type is an obscure list type designed to have an O(1) monoid operation.
    -- We use it here because this is important for parallelism.
    -- I can make this code much simpler/more idiomatic by adding a "Partionable" instance to "CoverTree".

    -- sort results into original order
    let sortedResults :: [[Neighbor (Labeled' dp l)]]
        sortedResults
            = map (\x -> [snd x])
            . sortBy (\(Labeled' _ y1,_) (Labeled' _ y2,_) -> compare y1 y2)
            $ toList res
    time "sorting results" sortedResults

    -- output distances
    let distanceL = map (map neighborDistance) sortedResults

    timeIO "outputing distance" $ do
        hDistances <- openFile (distances_file params) WriteMode
        forM_ distanceL (hPutStrLn hDistances . init . tail . show)
        hClose hDistances

    -- output neighbors
    let neighborL = (map (map (yLabeled' . neighbor))) sortedResults

    timeIO "outputting neighbors" $ do
        hNeighbors <- openFile (neighbors_file params) WriteMode
        forM_ neighborL (hPutStrLn hNeighbors . init . tail . show)
        hClose hNeighbors

    putStrLn "done"

-- | Gives us many possible ways to construct our cover trees based on the input parameters.
-- This is important so we can compare their runtime features.
buildTree :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    , Unboxable dp
    , Unbox dp
    , NFData (childC (CoverTree_ exprat childC leafC dp))
    ) => Params
      -> UArray dp
      -> IO (CoverTree_ exprat childC leafC dp)
buildTree params xs = do

    setExprat $ P.toRational $ expansionRatio params

    let trainmethod = case treetype params of
            Orig        -> inline trainInsertOrig
            Simplified  -> inline trainInsertNoSort
            Sort        -> inline trainInsert addChild_nothing
            Parent      -> inline trainInsert addChild_parent
            Ancestor    -> inline trainInsert addChild_ancestor
            TrainMonoid -> inline trainMonoid

    let (Just' reftree) = parallel trainmethod $ toList xs
    time "building tree" reftree

    -- Everything below here uses a sequential algorithm.
    -- These run faster if we disable multithreading in GHC 7.10.
    disableMultithreading $ do

        let reftree_adopt = if adopt_children params
                then ctAdoptNodes reftree
                else reftree
        time "adopting" reftree_adopt

        let reftree_sort = case sortMethod params of
                NoSort -> reftree_adopt
                NumDP_Distance  -> sortChildren cmp_numdp_distance  reftree_adopt
                NumDP_Distance' -> sortChildren cmp_numdp_distance' reftree_adopt
                Distance_NumDP  -> sortChildren cmp_distance_numdp  reftree_adopt
                Distance_NumDP' -> sortChildren cmp_distance_numdp' reftree_adopt
        time "sorting children" reftree_sort

        let reftree_prune = case packMethod params of
                NoPack    -> reftree_sort
                SetLeaves -> setLeaves 0 $ reftree_sort
                PackCT    -> packCT $ setLeaves 0 $ reftree_sort
        time "packing reference tree" reftree_prune

        let reftree_cache = if cache_dists params
                then setMaxDescendentDistance reftree_prune
                else reftree_prune
        time "caching distances" reftree_cache

        when (verbose params) $ do
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

--------------------------------------------------------------------------------
-- FIXME:
-- The code below should find a better home in subhask somewhere.

{-# RULES

"subhask/parallelInterleaved"       forall (f :: a -> ParList b). parallel f = parallelInterleaved f

  #-}

-- | See note above about ParList.
newtype ParList a = ParList [[a]]
    deriving (Read,Show,NFData)

mkMutable [t| forall a. ParList a |]

type instance Scalar (ParList a) = Int
type instance Logic (ParList a) = Bool
type instance Elem (ParList a) = a

instance (Eq a, Arbitrary a) => Arbitrary (ParList a) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (ParList a) where
    {-# INLINE size #-}
    size (ParList xs) = sum $ map length xs

instance Eq a => Eq_ (ParList a) where
    {-# INLINE (==) #-}
    (ParList a1)==(ParList a2) = a1==a2

instance POrd a => POrd_ (ParList a) where
    {-# INLINE inf #-}
    inf (ParList a1) (ParList a2) = ParList [inf (P.concat a1) (P.concat a2)]

instance POrd a => MinBound_ (ParList a) where
    {-# INLINE minBound #-}
    minBound = ParList []

instance Semigroup (ParList a) where
    {-# INLINE (+) #-}
    (ParList a1)+(ParList a2) = ParList $ a1 + a2

instance Monoid (ParList a) where
    {-# INLINE zero #-}
    zero = ParList []

instance Abelian (ParList a)

-- instance Eq a => Container (ParList a) where
--     {-# INLINE elem #-}
--     elem e (ParList a) = or $ map (elem e)

instance Constructible (ParList a) where
    {-# INLINE cons #-}
    {-# INLINE snoc #-}
    {-# INLINE singleton #-}
    {-# INLINE fromList1 #-}
    cons e (ParList [])     = ParList [[e]]
    cons e (ParList (x:xs)) = ParList ((e:x):xs)

    singleton e = ParList [[e]]

    fromList1 x xs = ParList [x:xs]

instance ValidEq a => Foldable (ParList a) where

    {-# INLINE toList #-}
    toList (ParList a) = P.concat a

--     {-# INLINE uncons #-}
--     uncons (ParList a) = if ParList.null a
--         then Nothing
--         else Just (ParList.index a 0, ParList $ ParList.drop 1 a)
--
--     {-# INLINE unsnoc #-}
--     unsnoc (ParList e) = if ParList.null e
--         then Nothing
--         else Just (ParList $ ParList.take (ParList.length e-1) e, ParList.index e 0)

--     foldMap f   (ParList a) = F.foldMap f   a
--
--     {-# INLINE foldr #-}
--     {-# INLINE foldr' #-}
--     {-# INLINE foldr1 #-}
--     foldr   f e (ParList a) = F.foldr   f e a
--     foldr'  f e (ParList a) = F.foldr'  f e a
--     foldr1  f   (ParList a) = F.foldr1  f   a
-- --     foldr1' f   (ParList a) = F.foldr1' f   a
--
--     {-# INLINE foldl #-}
--     {-# INLINE foldl' #-}
--     {-# INLINE foldl1 #-}
--     foldl   f e (ParList a) = F.foldl   f e a
--     foldl'  f e (ParList a) = F.foldl'  f e a
--     foldl1  f   (ParList a) = F.foldl1  f   a
-- --     foldl1' f   (ParList a) = F.foldl1' f   a

instance (ValidEq a) => Partitionable (ParList a) where
    {-# INLINABLE partition #-}
    partition n (ParList xs) = map (\x -> ParList [x]) $ partition n $ P.concat xs

    {-# INLINABLE partitionInterleaved #-}
    partitionInterleaved n (ParList xs) = map (\x -> ParList [x]) $ partitionInterleaved n $ P.concat xs
