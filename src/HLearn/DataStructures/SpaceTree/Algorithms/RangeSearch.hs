{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
    where

import Debug.Trace

import qualified Data.Map.Strict as Map

import SubHask
import SubHask.Monad
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- Range

data Range dp = Range
    { rangedp :: !dp
    , rangedistance :: !(Scalar dp)
    }

type instance Logic (Range dp) = Bool

deriving instance (Read dp, Read (Scalar dp)) => Read (Range dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (Range dp)

instance NFData (Range dp) where
    rnf dp = seq dp ()

instance Eq (Scalar dp) => Eq_ (Range dp) where
    r1 == r2 = rangedistance r1 == rangedistance r2

instance Ord (Scalar dp) => POrd_ (Range dp) where
    inf r1 r2 = if rangedistance r1 < rangedistance r2
        then r1
        else r2

instance Ord (Scalar dp) => Lattice_ (Range dp) where
    sup r1 r2 = if rangedistance r1 > rangedistance r2
        then r1
        else r2

instance Ord (Scalar dp) => Ord_ (Range dp)

---------------------------------------

{-# INLINABLE findRangeList #-}
findRangeList ::
    ( SpaceTree t dp
    , Eq dp
    , CanError (Scalar dp)
    ) => t dp -> Scalar dp -> dp -> [dp]
findRangeList tree maxdist query =
    prunefold (rl_prune maxdist query) (rl_cata maxdist query) [] tree

{-# INLINABLE rl_prune #-}
rl_prune ::
    ( SpaceTree t dp
    , Ord (Scalar dp)
    ) => Scalar dp -> dp -> [dp] -> t dp -> Bool
rl_prune maxdist query xs tree =
    stMinDistanceDp tree query > maxdist

{-# INLINABLE rl_cata #-}
rl_cata ::
    ( MetricSpace dp
    , Logic dp~Bool
    ) => Scalar dp -> dp -> dp -> [dp] -> [dp]
rl_cata maxdist query dp xs = if distance dp query < maxdist
    then dp:xs
    else xs

-- {-# INLINABLE findRangeList #-}
-- findRangeList ::
--     ( SpaceTree t dp
--     , Eq dp
--     , CanError (Scalar dp)
--     ) => t dp -> Scalar dp -> dp -> [dp]
-- findRangeList tree maxdist query =
--     prunefoldB_CanError (rl_catadp maxdist query) (rl_cata maxdist query) [] tree
--
-- {-# INLINABLE rl_catadp #-}
-- rl_catadp ::
--     ( MetricSpace dp
--     , CanError (Scalar dp)
--     , Ord (Scalar dp)
--     ) => Scalar dp -> dp -> dp -> [dp] -> [dp]
-- rl_catadp !maxdist !query !dp !rl = {-# SCC rl_catadp #-}
--     if isError dist
--         then rl
--         else dp:rl
--     where
--         dist = isFartherThanWithDistanceCanError dp query maxdist
--
-- {-# INLINABLE rl_cata #-}
-- rl_cata ::
--     ( SpaceTree t dp
--     , CanError (Scalar dp)
--     , Eq dp
--     ) => Scalar dp -> dp -> t dp -> [dp] -> [dp]
-- rl_cata !maxdist !query !tree !rl = {-# SCC rl_cata #-}
--     if isError dist
--         then errorVal
--         else if isFartherThan (stNode tree) query maxdist
--             then rl
--             else stNode tree:rl
--     where
--         dist = stIsMinDistanceDpFartherThanWithDistanceCanError tree query maxdist


------------
---- test

-- instance MetricSpace (Double,Double) where
--     distance (a1,a2) (b1,b2) = sqrt $ (a1-b1)**2 + (a2-b2)**2
--
-- instance POrd_ (Double,Double) where
--     inf (a1,a2) (b1,b2) = (inf a1 b1, inf a2 b2)
--
-- instance SupSemilattice (Double,Double) where
--     sup (a1,a2) (b1,b2) = (sup a1 b1, sup a2 b2)
--
-- instance Lattice (Double,Double) where
--
-- instance POrd (Double,Double) where
--     pcompare (a1,a2) (b1,b2) = case pcompare a1 a2 of
--         PEQ -> pcompare b1 b2
--         _   -> pcompare a1 a2
--
-- instance Ord (Double,Double)
