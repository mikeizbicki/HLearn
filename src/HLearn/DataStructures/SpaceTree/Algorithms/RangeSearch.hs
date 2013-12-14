{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
    where

import Debug.Trace

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import GHC.TypeLits
import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- Range

data Range dp = Range
    { rangedp :: !dp
    , rangedistance :: !(Ring dp)
    }

deriving instance (Read dp, Read (Ring dp)) => Read (Range dp)
deriving instance (Show dp, Show (Ring dp)) => Show (Range dp)

instance Eq (Ring dp) => Eq (Range dp) where
    r1 == r2 = rangedistance r1 == rangedistance r2

instance Ord (Ring dp) => Ord (Range dp) where
    compare r1 r2 = compare (rangedistance r1) (rangedistance r2)

instance NFData (Range dp) where
    rnf dp = seq dp ()

-------------------------------------------------------------------------------
-- RangeList

data RangeList dp = RangeList
    { mindist  :: !(Ring dp)
    , maxdist  :: !(Ring dp)
    , rangeset :: !(Set.Set (Range dp))
    }

instance NFData (Ring dp) => NFData (RangeList dp) where
    rnf rl = seq rl $ rnf (rangeset rl)

mkRangeList :: Ord (Ring dp) => Ring dp -> Ring dp -> RangeList dp
mkRangeList !a !b = RangeList a b mempty

rlInsert :: Ord (Ring dp) => Range dp -> RangeList dp -> RangeList dp
rlInsert !dp !rl = if rangedistance dp <= maxdist rl && rangedistance dp > mindist rl
    then rl { rangeset = Set.insert dp $ rangeset rl }
    else rl

instance ( Fractional (Ring dp), Ord (Ring dp)) =>  Monoid (RangeList dp) where
    mempty = RangeList
        { mindist = 0
        , maxdist = infinity
        , rangeset = mempty
        }

    mappend !rl1 !rl2 = RangeList
        { mindist = mindist' 
        , maxdist = maxdist'
        , rangeset = Set.filter (\r -> rangedistance r>mindist' && rangedistance r<maxdist') 
            $ rangeset rl1 <> rangeset rl2
        }
        where
            mindist' = max (mindist rl1) (mindist rl2)
            maxdist' = min (maxdist rl1) (maxdist rl2)

---------------------------------------

{-# INLINABLE findRangeList #-}
findRangeList :: (SpaceTree t dp, Eq dp) => t dp -> Ring dp -> Ring dp -> dp -> RangeList dp
findRangeList tree mindist maxdist query = 
    prunefoldB (rl_catadp query) (rl_cata query) (mkRangeList mindist maxdist) tree

{-# INLINABLE rl_catadp #-}
rl_catadp :: (MetricSpace dp, Ord (Ring dp)) => dp -> dp -> RangeList dp -> RangeList dp
rl_catadp !query !dp !rl = {-# SCC rl_catadp #-} 
    case isFartherThanWithDistance dp query (maxdist rl) of
        Strict.Nothing -> rl
        Strict.Just dist -> rlInsert (Range dp dist) rl

{-# INLINABLE rl_cata #-}
rl_cata :: forall k t dp. ( SpaceTree t dp, Eq dp ) => 
    dp -> t dp -> RangeList dp -> Strict.Maybe (RangeList dp)
rl_cata !query !tree !rl = {-# SCC rl_cata #-} 
    case stIsMinDistanceDpFartherThanWithDistance tree query (maxdist rl) of
        Strict.Nothing -> Strict.Nothing
        Strict.Just dist -> Strict.Just $ rlInsert (Range (stNode tree) dist) rl

-------------------------------------------------------------------------------
-- RangeMap

newtype RangeMap dp = RangeMap { rm2map :: Map.Map dp (RangeList dp) } 

deriving instance (NFData dp, NFData (Ring dp)) => NFData (RangeMap dp)

instance (Ord dp, Ord (Ring dp), Fractional (Ring dp)) => Monoid (RangeMap dp) where
    mempty = RangeMap mempty
    mappend !(RangeMap rm1) !(RangeMap rm2) = RangeMap $ Map.unionWith (undefined) rm1 rm2

---------------------------------------

findRangeMap :: (NFData (Ring dp), NFData dp, SpaceTree t dp, Ord dp) => Ring dp -> Ring dp -> DualTree (t dp) -> RangeMap dp
findRangeMap mindist maxdist dual = reduce $ 
    map (\dp -> RangeMap $ Map.singleton dp $ findRangeList (reference dual) mindist maxdist dp) (stToList $ query dual)

