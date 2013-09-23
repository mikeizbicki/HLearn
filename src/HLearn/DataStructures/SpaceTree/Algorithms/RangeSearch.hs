{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
    where

import Debug.Trace

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.TypeLits
import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- data structures

newtype RangeSearch (mindist::TypeFloat) (maxdist::TypeFloat) dp = RangeSearch 
    { getrange :: Set.Set dp
    }
    deriving (Read,Show,Monoid)

deriving instance NFData dp => NFData (RangeSearch mindist maxdist dp)

---------------------------------------

newtype RangeSearch2 (mindist::TypeFloat) (maxdist::TypeFloat) dp = RangeSearch2 
    { rsmap :: Map.Map dp (RangeSearch mindist maxdist dp) 
    }
    deriving (Read,Show)

deriving instance NFData dp => NFData (RangeSearch2 mindist maxdist dp)

instance 
    ( SingI mindist
    , SingI maxdist
    , SpaceTree tree dp
    , Ord dp
    ) => Function (RangeSearch2 mindist maxdist dp) (DualTree (tree dp)) (RangeSearch2 mindist maxdist dp) 
        where
    function _ = rangesearch

-------------------------------------------------------------------------------
-- algebra

instance Ord dp => Monoid (RangeSearch2 mindist maxdist dp) where
    mempty = RangeSearch2 mempty
    mappend (RangeSearch2 m1) (RangeSearch2 m2) = RangeSearch2 $ Map.unionWith (<>) m1 m2 

-------------------------------------------------------------------------------
-- traversals
 
rangesearch :: forall mindist maxdist t dp. 
    ( SingI mindist
    , SingI maxdist
    , SpaceTree t dp
    , Ord dp
    ) => DualTree (t dp) -> RangeSearch2 mindist maxdist dp
rangesearch dual = prunefold2init initRangeSearch2 range_prune range_cata dual

rangesearch_slow :: forall mindist maxdist t dp. 
    ( SingI mindist
    , SingI maxdist
    , SpaceTree t dp
    , Ord dp
    ) => DualTree (t dp) -> RangeSearch2 mindist maxdist dp
rangesearch_slow dual = prunefold2init initRangeSearch2 noprune range_cata dual

initRangeSearch2 :: forall mindist maxdist t dp. 
    ( SingI mindist
    , SingI maxdist
    , SpaceTree t dp
    , Ord dp
    ) => DualTree (t dp) -> RangeSearch2 mindist maxdist dp
initRangeSearch2 dual = if dist > mindist && dist < maxdist 
    then RangeSearch2 $ Map.singleton (stNode $ query dual) $ RangeSearch (Set.singleton $ stNode $ reference dual)
    else mempty
    where
        dist = distance (stNode $ reference dual) (stNode $ query dual)
        mindist = fromSing (sing :: Sing mindist)
        maxdist = fromSing (sing :: Sing maxdist)

range_prune :: forall mindist maxdist t dp.
    ( SpaceTree t dp 
    , SingI mindist
    , SingI maxdist
    ) => RangeSearch2 mindist maxdist dp -> DualTree (t dp) -> Bool
range_prune rs dual = stMinDistance (reference dual) (query dual) > maxdist
                   || stMaxDistance (reference dual) (query dual) < mindist
    where
        mindist = fromSing (sing :: Sing mindist)
        maxdist = fromSing (sing :: Sing maxdist)

range_cata :: forall mindist maxdist dp.
    ( SingI mindist
    , SingI maxdist
    , Ord dp
    , MetricSpace dp
    ) => DualTree dp -> RangeSearch2 mindist maxdist dp -> RangeSearch2 mindist maxdist dp
range_cata dual rs = if dist > mindist && dist < maxdist
    then RangeSearch2 $ Map.insertWith (<>) (query dual) (RangeSearch $ Set.singleton $ reference dual) $ rsmap rs
    else rs
    where
        dist = distance (reference dual) (query dual)
        mindist = fromSing (sing :: Sing mindist)
        maxdist = fromSing (sing :: Sing maxdist)

