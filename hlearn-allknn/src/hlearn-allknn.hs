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

import Control.DeepSeq
import Control.Monad
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Params as P
import Data.Params.Vector
import Data.Params.PseudoPrim
import qualified Data.Params.Vector.Unboxed as VPU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector.Algorithms.Intro as Intro
import Numeric
import System.Console.CmdArgs.Implicit
import System.IO

import Test.QuickCheck hiding (verbose,sample)
import Control.Parallel.Strategies

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import qualified HLearn.DataStructures.StrictList as Strict
import qualified HLearn.DataStructures.StrictVector as Strict
import HLearn.Metrics.Lebesgue
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.Normal
import HLearn.Models.Distributions

import Paths_HLearn
import Data.Version

import LoadData
import Timing 
import HLearn.UnsafeVector

type DP = L2 VU.Vector Float
type Tree = AddUnit (CoverTree' (13/10) V.Vector VU.Vector) () DP
 
-- type DP = L2' (VPU.Vector P.Automatic) Float
-- type Tree = AddUnit (CoverTree' (13/10) V.Vector (VPU.Vector P.RunTime)) () DP
-- 
-- instance FromRecord (VPU.Vector P.Automatic Float) where
--     parseRecord r = fmap VG.convert (parseRecord r :: Parser (V.Vector Float))
-- 
-- instance PseudoPrim (v a) => PseudoPrim (L2' v a) where
-- 
-- instance CK.Functor (VPU.Vector r) where
--     type FunctorConstraint (VPU.Vector r) a = VG.Vector (VPU.Vector r) a
--     fmap = VG.map
-- 
-- instance CK.Foldable (VPU.Vector r) where
--     type FoldableConstraint (VPU.Vector r) a = VG.Vector (VPU.Vector r) a
--     foldl' = VG.foldl'
--     foldr' = VG.foldr'
-- 
-- instance VG.Vector (VPU.Vector r) a => FromList (VPU.Vector r) a where
--     fromList = VG.fromList
--     toList = VG.toList
-- 
-- instance 
--     ( Param_len (VPU.Vector P.RunTime a)
--     , PseudoPrim a
--     ) => Monoid (VPU.Vector P.RunTime a) where
--     mempty = VG.empty
--     mappend a b = a -- VG.convert $ (VG.convert a :: V.Vector a) `mappend` (VG.convert b)

-------------------------------------------------------------------------------
-- command line parameters

data Params = Params
    { k                 :: Int
    , reference_file    :: Maybe String 
    , query_file        :: Maybe String
    , distances_file    :: String
    , neighbors_file    :: String 

    , pca_data          :: Bool
    , varshift_data     :: Bool
    , searchEpsilon     :: Float

    , packMethod        :: PackMethod
    , sortMethod        :: SortMethod

    , verbose           :: Bool
    , debug             :: Bool
    } 
    deriving (Show, Data, Typeable)

data PackMethod
    = NoPack
    | PackCT
    | PackCT2
    | PackCT3
    deriving (Eq,Read,Show,Data,Typeable)

data SortMethod
    = NoSort
    | NumDP_Distance
    | NumDP_Distance'
    | Distance_NumDP
    | Distance_NumDP'
    deriving (Eq,Read,Show,Data,Typeable)

allknnParams = Params 
    { k              = 1 
                    &= help "Number of nearest neighbors to find" 

    , reference_file = def 
                    &= help "Reference data set in CSV format" 
                    &= typFile

    , query_file     = def 
                    &= help "Query data set in CSV format" 
                    &= typFile 

    , distances_file = "distances_hlearn.csv" 
                    &= help "File to output distances into" 
                    &= typFile

    , neighbors_file = "neighbors_hlearn.csv" 
                    &= help "File to output the neighbors into" 
                    &= typFile

    , searchEpsilon   = 0
                    &= help ""
                    &= groupname "Approximations"

    , packMethod     = PackCT
                    &= help "Specifies which method to use for cache layout of the covertree"
                    &= groupname "Tree structure optimizations"

    , sortMethod     = NumDP_Distance
                    &= help "What order should the children be sorted in?"

    , pca_data       = False 
                    &= groupname "Data Preprocessing" 
                    &= help "Rotate the data points using the PCA transform.  Speeds up nearest neighbor searches, but computing the PCA can be expensive in many dimensions."
                    &= name "pca"
                    &= explicit

    , varshift_data  = False 
                    &= help "Sort the attributes according to their variance.  Provides almost as much speed up as the PCA transform during neighbor searches, but much less expensive in higher dimensions." 
                    &= name "varshift"
                    &= explicit

    , verbose        = False 
                    &= help "Print tree statistics (takes some extra time)" 
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
    -- cmd line args
    params <- cmdArgs allknnParams

    let checkfail x t = if x then error t else return ()
    checkfail (reference_file params == Nothing) "must specify a reference file"
    checkfail (searchEpsilon params < 0) "search epsilon must be >= 0"

    case k params of 
        1 -> runit params (undefined :: Tree) (undefined :: NeighborMap 1 DP)
--         2 -> runit params (undefined :: Tree) (undefined :: NeighborMap 2 DP)
--         3 -> runit params (undefined :: Tree) (undefined :: NeighborMap 3 DP)
--         4 -> runit params (undefined :: Tree) (undefined :: NeighborMap 4 DP)
--         5 -> runit params (undefined :: Tree) (undefined :: NeighborMap 5 DP)
--         6 -> runit params (undefined :: Tree) (undefined :: NeighborMap 6 DP)
--         7 -> runit params (undefined :: Tree) (undefined :: NeighborMap 7 DP)
--         8 -> runit params (undefined :: Tree) (undefined :: NeighborMap 8 DP)
--         9 -> runit params (undefined :: Tree) (undefined :: NeighborMap 9 DP)
--         10 -> runit params (undefined :: Tree) (undefined :: NeighborMap 10 DP)
        otherwise -> error "specified k value not supported"

{-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 1 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 2 DP -> IO ()#-}
{-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 3 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 4 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 5 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 6 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 7 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 8 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 9 DP -> IO ()#-}
-- {-# SPECIALIZE runit :: Params -> Tree -> NeighborMap 10 DP -> IO ()#-}

runit :: forall k tree base childContainer nodeVvec dp ring. 
    ( MetricSpace dp
    , Ord dp
    , KnownNat k
    , Show dp
    , Show (Scalar dp)
    , NFData dp
    , NFData (Scalar dp)
    , RealFloat (Scalar dp)
    , FromRecord dp 
    , VU.Unbox (Scalar dp)
--     , Param_len (VPU.Vector P.RunTime (L2' (VPU.Vector P.Automatic) Float))
    , dp ~ DP
    ) => Params 
      -> AddUnit (CoverTree' base childContainer nodeVvec) () dp 
      -> NeighborMap k dp 
      -> IO ()
runit params tree knn = do
    
    -- build reference tree
    let dataparams = DataParams
            { datafile = fromJust $ reference_file params
            , pca      = pca_data params
            , varshift = varshift_data params
            }
    rs <- loaddata dataparams

    let reftree = {-parallel-} train rs :: Tree
    timeIO "building reference tree" $ return reftree

    let reftree_sort = case sortMethod params of
            NoSort -> unUnit reftree
            NumDP_Distance  -> sortChildren cmp_numdp_distance  $ unUnit reftree 
            NumDP_Distance' -> sortChildren cmp_numdp_distance' $ unUnit reftree 
            Distance_NumDP  -> sortChildren cmp_distance_numdp  $ unUnit reftree 
            Distance_NumDP' -> sortChildren cmp_distance_numdp' $ unUnit reftree 
    timeIO "sorting children" $ return reftree_sort

    let reftree_prune = case packMethod params of
            NoPack -> reftree_sort
            PackCT -> packCT $ reftree_sort
            PackCT2 -> packCT2 20 $ reftree_sort
            PackCT3 -> packCT3 $ reftree_sort
    timeIO "packing reference tree" $ return reftree_prune

    -- verbose prints tree stats
    if verbose params 
        then do
            putStrLn ""
            printTreeStats "reftree      " $ unUnit reftree 
            printTreeStats "reftree_prune" $ reftree_prune
        else return ()

    -- build query tree
    (querytree,qs) <- case query_file params of
        Nothing -> return $ (reftree_prune,rs)
        Just qfile -> do
            qs <- loaddata $ dataparams { datafile = qfile }
            let qtree = train qs :: Tree
            timeIO "building query tree" $ return qtree
            let qtree_prune = packCT $ unUnit qtree
            timeIO "packing query tree" $ return qtree_prune
            return (qtree_prune,qs)

    -- do knn search
--     let result = findNeighborVec (DualTree (reftree_prune) (querytree)) :: V.Vector (NeighborList k DP)
--     let result = findNeighborSL (DualTree (reftree_prune) (querytree)) :: Strict.List (NeighborList k DP)
    let result = parFindEpsilonNeighborMap 
            ( searchEpsilon params ) 
            ( DualTree 
                ( reftree_prune ) 
                ( querytree )
            ) 
            :: NeighborMap k DP

    res <- timeIO "computing parFindNeighborMap" $ return result

    -- output to files
    let qs_index = Map.fromList $ zip (VG.toList qs) [0::Int ..]
        rs_index = Map.fromList $ zip (VG.toList rs) [0::Int ..]

--     timeIO "outputing distance" $ do
--         hDistances <- openFile (distances_file params) WriteMode
--         sequence_ $ 
--             map (hPutStrLn hDistances . concat . intersperse "," . map (\x -> showEFloat (Just 10) x "")) 
--             . Map.elems 
--             . Map.mapKeys (\k -> fromJust $ Map.lookup k qs_index) 
--             . Map.map (map neighborDistance . getknnL) 
--             $ nm2map res 
--         hClose hDistances
  
    timeIO "outputing neighbors" $ do
        hNeighbors <- openFile (neighbors_file params) WriteMode
        sequence_ $ 
            map (hPutStrLn hNeighbors . init . tail . show)
            . Map.elems 
            . Map.map (map (\v -> fromJust $ Map.lookup v rs_index)) 
            . Map.mapKeys (\k -> fromJust $ Map.lookup k qs_index) 
            . Map.map (map neighbor . getknnL) 
--             $ Map.fromList $ V.toList $ V.imap (\i x -> (stNodeV querytree VG.! i,x)) res 
--             $ Map.fromList $ zip (stToList querytree) (Strict.strictlist2list res)
            $ nm2map res 
        hClose hNeighbors
    -- end
    putStrLn "end"

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
    putStr (str++"  stMaxNodeV...........") >> hFlush stdout >> putStrLn (show $ stMaxNodeV t) 
    putStr (str++"  stAveNodeV...........") >> hFlush stdout >> putStrLn (show $ mean $ stAveNodeV t) 
    putStr (str++"  stMaxChildren........") >> hFlush stdout >> putStrLn (show $ stMaxChildren t) 
    putStr (str++"  stAveChildren........") >> hFlush stdout >> putStrLn (show $ mean $ stAveChildren t) 
    putStr (str++"  stMaxDepth...........") >> hFlush stdout >> putStrLn (show $ stMaxDepth t) 
    putStr (str++"  stNumSingletons......") >> hFlush stdout >> putStrLn (show $ stNumSingletons t) 
    putStr (str++"  stExtraLeaves........") >> hFlush stdout >> putStrLn (show $ stExtraLeaves t) 

    putStrLn (str++" properties:")
    putStr (str++"  covering...............") >> hFlush stdout >> putStrLn (show $ property_covering $ UnitLift t) 
    putStr (str++"  leveled................") >> hFlush stdout >> putStrLn (show $ property_leveled $ UnitLift t) 
    putStr (str++"  separating.............") >> hFlush stdout >> putStrLn (show $ property_separating $ UnitLift t)
    putStr (str++"  maxDescendentDistance..") >> hFlush stdout >> putStrLn (show $ property_maxDescendentDistance $ UnitLift t) 

    putStrLn ""
