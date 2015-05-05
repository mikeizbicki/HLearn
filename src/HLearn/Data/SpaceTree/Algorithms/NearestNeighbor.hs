{-# LANGUAGE DataKinds,UnboxedTuples,MagicHash,TemplateHaskell,RankNTypes,TupleSections #-}

module HLearn.Data.SpaceTree.Algorithms.NearestNeighbor
    (

    -- * data types
    Neighbor (..)
    , ValidNeighbor (..)

    , NeighborList (..)
    , getknnL
--     , nlMaxDist

    -- * functions
    , findAllNeighbors
    )
    where

import qualified Prelude as P
import Data.Strict.Tuple (Pair(..))

import SubHask
import SubHask.Algebra.Container
import SubHask.Compatibility.Containers
import SubHask.Compatibility.StaticVector
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.SpaceTree

-- import Data.Params
import HLearn.Data.SpaceTree.CoverTree
import SubHask.Compatibility.Vector
import SubHask.Compatibility.Vector.Lebesgue
import qualified Data.Vector.Generic as VG
import GHC.Exts (inline)

-------------------------------------------------------------------------------

data Neighbor dp = Neighbor
    { neighbor         :: !dp
    , neighborDistance :: !(Scalar dp)
--     { neighbor         :: {-#UNPACK#-}!(Labeled' (L2 UnboxedVector Float) Int)
--     , neighborDistance :: {-#UNPACK#-}!Float
    }

type ValidNeighbor dp = (Metric dp, Bounded (Scalar dp))

type instance Logic (Neighbor dp) = Bool

deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf (Neighbor _ _) = ()

instance (Eq dp, Eq (Scalar dp)) => Eq_ (Neighbor dp) where
    (Neighbor dp1 dist1)==(Neighbor dp2 dist2) = dp1==dp2 && dist1==dist2

------------------------------------------------------------------------------

data NeighborList (k :: Nat) dp
    = NL_Nil
    | NL_Cons {-#UNPACK#-}!(Neighbor dp) !(NeighborList k dp)

type instance Scalar (NeighborList k dp) = Int
type instance Logic (NeighborList k dp) = Bool

mkMutable [t| forall k dp. NeighborList k dp |]

instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborList k dp) where
    rnf NL_Nil = ()
    rnf (NL_Cons n ns) = ()

instance (ValidNeighbor dp, Eq_ dp) => Eq_ (NeighborList k dp) where
    (NL_Cons x xs) == (NL_Cons y ys) = x==y && xs==ys
    NL_Nil         == NL_Nil         = True
    _              == _              = False

property_orderedNeighborList :: (Logic dp~Bool, Metric dp) => NeighborList k dp -> Bool
property_orderedNeighborList NL_Nil = True
property_orderedNeighborList (NL_Cons n NL_Nil) = True
property_orderedNeighborList (NL_Cons n (NL_Cons n2 ns)) = if neighborDistance n < neighborDistance n2
    then property_orderedNeighborList (NL_Cons n2 ns)
    else False

{-# INLINABLE getknnL #-}
getknnL :: NeighborList k dp -> [Neighbor dp]
getknnL (NL_Cons x xs) = x:getknnL xs
getknnL NL_Nil = []

{-# INLINABLE nlMaxDist #-}
nlMaxDist ::
    ( ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp
nlMaxDist !nl = {-# SCC nlMaxDist #-} go nl
    where
        go (NL_Cons n NL_Nil) = neighborDistance n
        go (NL_Cons n ns) = go ns
        go NL_Nil = maxBound

instance
    ( ValidNeighbor dp
    ) => Monoid (NeighborList k dp)
        where

    {-# INLINABLE zero #-}
    zero = NL_Nil

instance
    ( ValidNeighbor dp
    ) => Semigroup (NeighborList k dp)
        where

    {-# INLINABLE (+) #-}
    nl1    + NL_Nil = nl1
    NL_Nil + nl2    = nl2
    nl1    + nl2    = {-# SCC plus_NeighborList #-} ret
        where
            -- FIXME:
            -- typeparams doesn't work with GHC 7.10,
            -- so I'm hard coding the number of neighbors to check
--             ret = go nl1 nl2 (viewParam _k nl1)
            ret = go nl1 nl2 (1::Int)

            go _ _ 0 = NL_Nil
            go (NL_Cons n1 ns1) (NL_Cons n2 ns2) k = if neighborDistance n1 > neighborDistance n2
                then NL_Cons n2 $ go (NL_Cons n1 ns1) ns2 (k-1)
                else NL_Cons n1 $ go ns1 (NL_Cons n2 ns2) (k-1)
            go NL_Nil (NL_Cons n2 ns2) k = NL_Cons n2 $ go NL_Nil ns2 (k-1)
            go (NL_Cons n1 ns1) NL_Nil k = NL_Cons n1 $ go ns1 NL_Nil (k-1)
            go NL_Nil NL_Nil k = NL_Nil

{-# INLINABLE nlAddNeighbor #-}
nlAddNeighbor :: forall k dp.
    ( ValidNeighbor dp
    ) => NeighborList k dp -> Neighbor dp -> NeighborList k dp
-- nlAddNeighbor (NL_Cons n NL_Nil) n' = {-# SCC nlAddNeighbor #-} if neighborDistance n' > neighborDistance n
--     then NL_Cons n NL_Nil
--     else NL_Cons n' NL_Nil
-- nlAddNeighbor NL_Nil n' = {-# SCC nlAddNeighbor #-} NL_Cons n' NL_Nil
nlAddNeighbor nl n' = nl+NL_Cons n' NL_Nil

-------------------------------------------------------------------------------
-- single tree

{-
{-# INLINABLE prunefoldB_CanError_sort #-}
prunefoldB_CanError_sort ::
    ( SpaceTree t a
    , ValidNeighbor a
    , b ~ NeighborList k a
    , ClassicalLogic a
    , CanError (Scalar a)
    , Bounded (Scalar a)
    ) =>
    a -> (a -> b -> b) -> (Scalar a -> t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError_sort !query !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_sort #-}
    go ( distance (stNode t) query :!: t ) b
    where
        go !( dist :!: t ) !b =  if isError res
            then b
            else foldr' go b'' children'
                where
                    res = f2 dist t b
                    b'' = foldr' f1 res (stLeaves t)

                    children'
                        = {-# SCC children' #-} qsortHalf (\( d1 :!: _ ) ( d2 :!: _ ) -> compare d2 d1)
                        $  map (\x -> ( stIsMinDistanceDpFartherThanWithDistanceCanError x query maxdist
                                      :!: x ))
--                         $ map (\x -> ( distanceUB (stNode x) query (lambda t+maxdist), x ))
--                         $ map (\x -> ( distance (stNode x) query , x ))
                        $ toList
                        $ stChildren t

                    maxdist = nlMaxDist b''

-- | This is a version of quicksort that only descends on its lower half.
-- That is, it only "approximately" sorts a list.
-- It is modified from http://en.literateprograms.org/Quicksort_%28Haskell%29
{-# INLINABLE qsortHalf #-}
qsortHalf :: (a -> a -> Ordering) -> [a] -> [a]
qsortHalf !cmp !x = {-# SCC qsortHalf #-} go x []
    where
        go [] !y     = y
        go [x] !y    = x:y
        go (x:xs) !y = part xs [] [x] []
            where
                part [] !l !e !g = go l (e ++ g ++ y)
                part (z:zs) !l !e !g = case cmp z x of
                    GT -> part zs l e (z:g)
                    LT -> part zs (z:l) e g
                    EQ -> part zs l (z:e) g
-}

---------------------------------------

{-# INLINE manualknn #-}
manualknn ::
    ( SpaceTree t dp
    , Index (LeafContainer t dp) ~ Int
    , Scalar (LeafContainer t dp) ~ Int
    , IxContainer (LeafContainer t dp)
    , Bounded (Scalar dp)
    ) => dp -> t dp -> Neighbor dp
manualknn q t = {-# SCC manualknn #-} go t $ Neighbor q maxBound
    where
        go !t (Neighbor n distn) = if dist > maxdist
            then Neighbor n distn
            else foldr' go leafres $ stChildren t
            where
                leafres = inline_foldr' nn_catadp nl' $ stLeaves t

                nl' = if dist==0 || dist >= distn
                    then Neighbor n distn
                    else Neighbor (stNode t) dist

                dist = distanceUB q (stNode t) maxdist
                maxdist = distn+lambda t

                {-# INLINE inline_foldr' #-}
                inline_foldr' !f !tot !v = {-# SCC inline_foldr' #-} go1 (size v-1) tot
                    where
                        go1 (-1) !tot = tot
                        go1 !i   !tot = go1 (i-1) $ f (v!i) tot

                {-# INLINE nn_catadp #-}
                nn_catadp !dp (Neighbor n distn) = {-# SCC nn_catadp #-}
                    if dist==0 || dist>distn
                        then Neighbor n distn
                        else Neighbor dp dist
                    where
                        dist = distanceUB q dp distn

{-# INLINE findAllNeighbors #-}
findAllNeighbors ::
    ( SpaceTree t dp
    , Index (LeafContainer t dp) ~ Int
    , Scalar (LeafContainer t dp) ~ Int
    , IxContainer (LeafContainer t dp)
    , Bounded (Scalar dp)
    ) => Scalar dp
      -> t dp
      -> [dp]
      -> Seq ( dp, NeighborList 1 dp )
findAllNeighbors epsilon rtree qs = reduce $ map
    (\dp -> singleton (dp, NL_Cons (manualknn dp rtree) NL_Nil ))
    qs
