{-# LANGUAGE ScopedTypeVariables #-}
module HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
    where

import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits
import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- data structures

data TypeFloat = (%) Nat Nat 

data instance Sing (n::TypeFloat) = SFloat Integer Integer

instance (SingI a, SingI b) => SingI ((%) a b) where
    sing = SFloat (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))

instance Fractional r => SingE (Kind :: TypeFloat) r where
    fromSing (SFloat a b) = fromIntegral a/fromIntegral b

---------------------------------------

newtype RangeSearch (mindist::TypeFloat) (maxdist::TypeFloat) dp = RangeSearch 
    { getrange :: Set.Set dp
    }
    deriving (Read,Show,Monoid)

newtype RangeSearch2 (mindist::TypeFloat) (maxdist::TypeFloat) dp = RangeSearch2 
    { rsmap :: Map.Map dp (RangeSearch mindist maxdist dp) 
    }
    deriving (Read,Show)

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
    , Show dp
    ) => DualTree (t dp) -> RangeSearch2 mindist maxdist dp
rangesearch dual = prunefold2init initRangeSearch2 range_prune range_cata dual

rangesearch_slow :: forall mindist maxdist t dp. 
    ( SingI mindist
    , SingI maxdist
    , SpaceTree t dp
    , Ord dp
    , Show dp
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
    , Show dp
    ) => DualTree dp -> RangeSearch2 mindist maxdist dp -> RangeSearch2 mindist maxdist dp
range_cata dual rs = if dist > mindist && dist < maxdist
    then RangeSearch2 $ Map.insertWith (<>) (query dual) (RangeSearch $ Set.singleton $ reference dual) $ rsmap rs
    else rs
    where
        dist = distance (reference dual) (query dual)
        mindist = fromSing (sing :: Sing mindist)
        maxdist = fromSing (sing :: Sing maxdist)

