{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.Console.CmdArgs.Implicit
import System.Environment
import System.IO

import Test.QuickCheck hiding (verbose,sample)
import Debug.Trace
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.Metrics.Lebesgue
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.Normal
import HLearn.Models.Distributions

import Utils

type DP = L2 V.Vector Double
type Tree = AddUnit (CoverTree' (5/4)) () DP

data Params = Params
    { k :: Int
    , label_column :: Int
    , data_file :: Maybe String 
    , verbose :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { k              = 1 &= help "number of nearest neighbors to find" 
    , label_column   = 0 &= help "column of the label in data_file"
    , data_file      = def &= help "reference data set in CSV format" &= typFile
    , verbose        = False &= help "print tree statistics (takes some extra time)"
    }
    &= summary "HLearn k-nearest neighbor classifier, version 1.0"


main = do
    -- cmd line args
    params <- cmdArgs sample

    let checkfail x t = if x then error t else return ()
    checkfail (data_file params == Nothing) "must specify a data file"

    case k params of 
        1 -> runit params (undefined :: Tree) (undefined :: KNN2 1 DP)
        otherwise -> error "specified k value not supported"

{-# SPECIALIZE runit :: Params -> Tree -> KNN2 1 DP -> IO ()#-}
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
    let ref = fromJust $ data_file params
    rs <- loaddata ref 
    
    let reftree = {-parallel-} train rs :: CoverTree DP 
    timeIO "building reference tree" $ return $ rnf reftree
    let reftree_prune = pruneExtraLeaves $ pruneSingletons $ unUnit reftree
    timeIO "pruning reference tree" $ return $ rnf reftree_prune

    -- distance metric



    -- end
    putStrLn "end"

