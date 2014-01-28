module HLearn.DataStructures.SpaceTree.Simple
    where

import Control.DeepSeq
import qualified Data.Foldable as F
import qualified Data.Strict as Strict
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor

-------------------------------------------------------------------------------
-- data types

newtype Simple v dp = Simple { unSimple :: v dp }
    deriving (Read,Show,Eq,Ord,NFData)

-------------------------------------------------------------------------------
-- algebra

instance VG.Vector v dp => Monoid (Simple v dp) where
    mempty = Simple $ VG.empty
    mappend s1 s2 = Simple $ unSimple s1 VG.++ unSimple s2

-------------------------------------------------------------------------------
-- training

instance VG.Vector v dp => HomTrainer (Simple v dp) where
    type Datapoint (Simple v dp) = dp
    train1dp dp = Simple $ VG.singleton dp
    train dps = Simple $ VG.fromList $ F.toList dps

-------------------------------------------------------------------------------
-- nn

simple_knn :: 
    ( MetricSpace dp
    , VG.Vector v dp
    , SingI k
    , Eq dp
    ) => dp -> NeighborList k dp -> Simple v dp -> NeighborList k dp
simple_knn query knn (Simple v) = VG.foldl' cata knn v
    where
        cata knn dp = case isFartherThanWithDistance query dp (nl_maxdist knn) of
            Strict.Nothing -> knn
            Strict.Just dist -> mkNeighborList dp dist <> knn

-- instance SpaceTree (Simple v) where
--     type NodeContainer = v
--     type ChildContainer = v
