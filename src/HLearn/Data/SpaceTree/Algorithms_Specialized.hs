-- | This module contains algorithms for efficient operations over space trees.
-- Currently, only nearest neighbor queries are implemented.
-- It would be easy to implement more query types, however.
-- If there is another query type you want supported, ask me and I'll implement it for you.
--
-- The paper <http://arxiv.org/abs/1304.4327 Tree Independent Dual Tree Algorithms> gives full details on possible queries.
module HLearn.Data.SpaceTree.Algorithms_Specialized
    (

    Neighbor (..)
    , ValidNeighbor (..)

    , findNeighbor
    , findNeighbor_NoSort
    )
    where

import GHC.Exts (inline)
import Data.List (sortBy)

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Container
import SubHask.Algebra.Vector
import SubHask.Compatibility.Containers
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.SpaceTree

-------------------------------------------------------------------------------

data Neighbor dp = Neighbor
--     { neighbor         :: !dp
--     , neighborDistance :: !(Scalar dp)
    { neighbor         :: !(Labeled' (UVector "dyn" Float) Int)
    , neighborDistance :: !Float
    }

type ValidNeighbor dp = dp~(Labeled' (UVector "dyn" Float) Int)

deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf (Neighbor _ _) = ()

type instance Logic (Neighbor dp) = Bool

instance (Eq dp, Eq (Scalar dp)) => Eq_ (Neighbor dp) where
    (Neighbor dp1 dist1)==(Neighbor dp2 dist2) = dist1==dist2 && dp1==dp2

----------------------------------------

-- | Find the nearest neighbor of a node.
--
-- NOTE:
-- If we remove the call to "inline" on "foldr'",
-- GHC 7.10 will pass dictionaries and everything becomes very slow.
{-# INLINE findNeighbor #-}
findNeighbor ::
    ( SpaceTree t dp
    , Bounded (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> t dp -> dp -> Neighbor dp
findNeighbor ε t q =
    {-# SCC findNeighbor #-}
    go (Labeled' t startdist) startnode
    where
        startnode = if startdist == 0
            then Neighbor q maxBound
            else Neighbor (stNode t) startdist

        startdist = distance (stNode t) q

        go (Labeled' t dist) (Neighbor n distn) = if dist*ε > maxdist
            then Neighbor n distn
            else inline foldr' go leafres
                $ sortBy (\(Labeled' _ d1) (Labeled' _ d2) -> compare d2 d1)
                $ map (\t' -> Labeled' t' (distanceUB q (stNode t') (distnleaf+stMaxDescendentDistance t)))
                $ toList
                $ stChildren t
            where
                leafres@(Neighbor _ distnleaf) = inline foldr'
                    (\dp n@(Neighbor _ distn') -> cata dp (distanceUB q dp distn') n)
                    (cata (stNode t) dist (Neighbor n distn))
                    (stLeaves t)

                maxdist = distn+stMaxDescendentDistance t

        cata !dp !dist (Neighbor n distn) =
            if dist==0 || dist>distn
                then Neighbor n distn
                else Neighbor dp dist

----------------------------------------

-- | Find the nearest neighbor of a node.
-- Internally, this function does not sort the distances of the children before descending.
-- In some (rare) cases this reduces the number of distance comparisons.
{-# INLINE findNeighbor_NoSort #-}
findNeighbor_NoSort ::
    ( SpaceTree t dp
    , Bounded (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> t dp -> dp -> Neighbor dp
findNeighbor_NoSort ε t q =
    {-# SCC findNeighbor_NoSort #-}
    go t (Neighbor q maxBound)
    where
        go t res@(Neighbor _ distn) = if dist*ε > maxdist
            then res
            else inline foldr' go leafres $ stChildren t
            where
                leafres = inline foldr'
                    (\dp n@(Neighbor _ distn') -> cata dp (distanceUB q dp distn') n)
                    (cata (stNode t) dist res)
                    (stLeaves t)

                dist = distanceUB q (stNode t) maxdist
                maxdist = distn+stMaxDescendentDistance t

        cata !dp !dist (Neighbor n distn) =
            if dist==0 || dist>distn
                then Neighbor n distn
                else Neighbor dp dist
