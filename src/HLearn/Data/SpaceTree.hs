{-# LANGUAGE DataKinds,MagicHash,UnboxedTuples #-}

module HLearn.Data.SpaceTree
    (
    -- * Type classes
    SpaceTree (..)

    , Weighted#

    -- * Generic algorithms
    , stMaxDescendentDistance
    , stToList
--     , stToListW
    , stDescendents
    , stNumDp
    , stNumNodes
    , stNumLeaves
    , stNumGhosts
    , stAveGhostChildren
    , stNumGhostSingletons
    , stNumGhostLeaves
    , stNumGhostSelfparent
    , stMaxLeaves
    , stAveLeaves
    , stMaxChildren
    , stAveChildren
    , stMaxDepth
    , stNumSingletons
    , stExtraLeaves

    -- * Pruning folds

    -- ** Single tree

    , prunefoldinit
    , prunefold
    , prunefoldA
    , prunefoldB
    , prunefoldB_CanError
--     , prunefoldB_CanError_sort
    , prunefoldC
    , prunefoldD
--     , prunefoldM
    , noprune

    )
    where

import Debug.Trace

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import Prelude (map)
import SubHask
import SubHask.Monad
import SubHask.Compatibility.Containers

import HLearn.Models.Distributions.Univariate.Normal

-------------------------------------------------------------------------------
-- SpaceTree

type Weighted# dp = (# Scalar dp, dp #)

fst# :: (# a,b #) -> a
fst# (# a,b #) = a

snd# :: (# a,b #) -> b
snd# (# a,b #) = b

class
    ( Metric dp
    , Logic (t dp) ~ Bool
    , Logic dp ~ Bool
    , Logic (LeafContainer t dp) ~ Bool
    , Logic (LeafContainer t (t dp)) ~ Bool
    , Logic (ChildContainer t (t dp)) ~ Bool
    , Eq_ (t dp)
    , Eq_ (ChildContainer t (t dp))
    , Scalar dp ~ Scalar (t dp)
    , Elem (ChildContainer t (t dp)) ~ t dp
    , Elem (LeafContainer t dp) ~ dp
    , Constructible (LeafContainer t dp)
    , Constructible  (ChildContainer t (t dp))
    , Normed (LeafContainer t dp)
    , Normed (ChildContainer t (t dp))
    , Foldable (LeafContainer t dp)
    , Foldable (ChildContainer t (t dp))
--     , Container (LeafContainer t dp)
--     , Container (LeafContainer t (t dp))

--     , Constructible (t dp)
    , dp ~ Elem (t dp)
    ) => SpaceTree t dp
        where

    type LeafContainer t :: * -> *
    type LeafContainer t = V.Vector

    type ChildContainer t :: * -> *
    type ChildContainer t = V.Vector

    {-# INLINABLE stMinDistance #-}
    {-# INLINABLE stMaxDistance #-}
    stMinDistance :: t dp -> t dp -> Scalar dp
    stMinDistance t1 t2 = fst# (stMinDistanceWithDistance t1 t2)
    stMaxDistance :: t dp -> t dp -> Scalar dp
    stMaxDistance t1 t2 = fst# (stMaxDistanceWithDistance t1 t2)

    stMinDistanceWithDistance :: t dp -> t dp -> (# Scalar dp, Scalar dp #)
    stMaxDistanceWithDistance :: t dp -> t dp -> (# Scalar dp, Scalar dp #)

    {-# INLINABLE stMinDistanceDp #-}
    {-# INLINABLE stMaxDistanceDp #-}
    stMinDistanceDp :: t dp -> dp -> Scalar dp
    stMinDistanceDp t dp = fst# (stMinDistanceDpWithDistance t dp)
    stMaxDistanceDp :: t dp -> dp -> Scalar dp
    stMaxDistanceDp t dp = fst# (stMaxDistanceDpWithDistance t dp)

    stIsMinDistanceDpFartherThanWithDistanceCanError
        :: CanError (Scalar dp) => t dp -> dp -> Scalar dp -> Scalar dp
    stIsMaxDistanceDpFartherThanWithDistanceCanError
        :: CanError (Scalar dp) => t dp -> dp -> Scalar dp -> Scalar dp

    stMinDistanceDpWithDistance :: t dp -> dp -> (# Scalar dp, Scalar dp #)
    stMaxDistanceDpWithDistance :: t dp -> dp -> (# Scalar dp, Scalar dp #)

    stMinDistanceDpFromDistance :: t dp -> dp -> Scalar dp -> Scalar dp
    stMaxDistanceDpFromDistance :: t dp -> dp -> Scalar dp -> Scalar dp

    stHasNode   :: t dp -> Bool
    stChildren  :: t dp -> ChildContainer t (t dp)
    stNode      :: t dp -> dp
    stWeight    :: t dp -> Scalar dp

    {-# INLINABLE stHasNoChildren #-}
    stHasNoChildren :: t dp -> Bool
    stHasNoChildren t = isEmpty $ stChildren t

    {-# INLINABLE stChildrenList #-}
    stChildrenList :: t dp -> [t dp]
    stChildrenList t = toList $ stChildren t

    {-# INLINABLE stLeaves #-}
    stLeaves :: t dp -> LeafContainer t dp
    stLeaves t = zero

    {-# INLINABLE stNodeW #-}
    stNodeW :: t dp -> Weighted# dp
    stNodeW t = (# stWeight t, stNode t #)

    ro :: t dp -> Scalar dp
    lambda :: t dp -> Scalar dp

-------------------------------------------------------------------------------
-- generic algorithms

{-# INLINABLE stMaxDescendentDistance #-}
stMaxDescendentDistance :: (Eq dp, SpaceTree t dp) => t dp -> Scalar dp
stMaxDescendentDistance t = maximum_ 0 $ map (distance (stNode t)) $ stDescendents t

{-# INLINABLE stToSeqDFS #-}
stToSeqDFS :: SpaceTree t dp => t dp -> Seq dp
stToSeqDFS t
    = stNode t `cons` (fromList $ toList $ stLeaves t)
    + (foldl' (+) empty $ map stToSeqDFS $ stChildrenList t)

{-# INLINABLE stToSeqBFS #-}
stToSeqBFS :: SpaceTree t dp => t dp -> Seq dp
stToSeqBFS t = stNode t `cons` go t
    where
        go t = (fromList $ toList $ stLeaves t)
             + (fromList $ map stNode $ toList $ stChildren t)
             + (foldl' (+) empty $ map go $ stChildrenList t)

{-# INLINABLE stToList #-}
stToList :: SpaceTree t dp => t dp -> [dp]
stToList = toList . stToSeqDFS

-- {-# INLINABLE stToList #-}
-- stToList :: (Eq dp, SpaceTree t dp) => t dp -> [dp]
-- stToList t = if stHasNoChildren t && stWeight t > 0
--     then (stNode t):(toList $ stLeaves t)
--     else go (concat $ map stToList $ stChildrenList t)
--     where
--         go xs = if stWeight t > 0
--             then (stNode t) : (toList (stLeaves t) ++ xs)
--             else toList (stLeaves t) ++ xs

-- {-# INLINABLE stToListW
-- stToListW :: (Eq dp, SpaceTree t dp) => t dp -> [Weighted dp]
-- stToListW t = if stHasNoChildren t && stWeight t > 0
--     then [(stWeight t,stNode t)]
--     else go (concat $ map stToListW $ stChildrenList t)
--     where
--         go xs = if stWeight t > 0
--             then (stWeight t,stNode t) : xs
--             else xs

-- {-# INLINABLE toTagList #-}
-- toTagList :: (Eq dp, SpaceTree (t tag) dp, Taggable t dp) => t tag dp -> [(dp,tag)]
-- toTagList t = if stHasNoChildren t
--     then [(stNode t,getTag t)]
--     else go (concat $ map toTagList $ stChildrenList t)
--     where
--         go xs = if stNode t `elem` (map stNode $ stChildrenList t)
--             then xs
--             else (stNode t,getTag t) : xs

{-# INLINABLE stDescendents #-}
stDescendents :: SpaceTree t dp => t dp -> [dp]
stDescendents t = case tailMaybe $ go t of
    Just xs -> xs
    where
        go t = stNode t : L.concatMap go (stChildrenList t) ++ toList (stLeaves t)

-- stDescendents t = if stHasNoChildren t
--     then [stNode t]
--     else L.concatMap stDescendents (stChildrenList t) ++ toList (stLeaves t)

{-# INLINABLE stNumDp #-}
stNumDp :: SpaceTree t dp => t dp -> Scalar dp
stNumDp t = if stHasNoChildren t
    then stWeight t
    else stWeight t + sum (map stNumDp $ stChildrenList t)

{-# INLINABLE stNumNodes #-}
stNumNodes :: SpaceTree t dp => t dp -> Int
stNumNodes t = if stHasNoChildren t
    then 1
    else 1 + sum (map stNumNodes $ stChildrenList t)

{-# INLINABLE stNumLeaves #-}
stNumLeaves ::
    ( Integral (Scalar (LeafContainer t dp))
    , SpaceTree t dp
    ) => t dp -> Scalar dp
stNumLeaves t = (fromIntegral $ size (stLeaves t)) + sum (map stNumLeaves $ toList $ stChildren t)

{-# INLINABLE stNumGhosts #-}
stNumGhosts :: SpaceTree t dp => t dp -> Int
stNumGhosts t = (if stWeight t == 0 then 1 else 0) + if stHasNoChildren t
    then 0
    else sum (map stNumGhosts $ stChildrenList t)

{-# INLINABLE stAveGhostChildren #-}
stAveGhostChildren :: SpaceTree t dp => t dp -> Normal Double
stAveGhostChildren t =
    ( if stWeight t == 0
        then train1Normal . fromIntegral . size $ stChildrenList t
        else zero
    )
    +
    ( if stHasNoChildren t
        then zero
        else (reduce . map stAveGhostChildren $ stChildrenList t)
    )

{-# INLINABLE stMaxLeaves #-}
stMaxLeaves :: SpaceTree t dp => t dp -> Int
stMaxLeaves t = maximum $ (size $ toList $ stLeaves t):(map stMaxLeaves $ stChildrenList t)

{-# INLINABLE stAveLeaves #-}
stAveLeaves :: SpaceTree t dp => t dp -> Normal Double
stAveLeaves t = (train1Normal . fromIntegral . size . toList $ stLeaves t)
              + (reduce . map stAveLeaves $ stChildrenList t)

{-# INLINABLE stMaxChildren #-}
stMaxChildren :: SpaceTree t dp => t dp -> Int
stMaxChildren t = if stHasNoChildren t
    then 0
    else maximum $ (size $ stChildrenList t):(map stMaxChildren $ stChildrenList t)

{-# INLINABLE stAveChildren #-}
stAveChildren :: SpaceTree t dp => t dp -> Normal Double
stAveChildren t = if stHasNoChildren t
    then zero
    else (train1Normal . fromIntegral . size $ stChildrenList t)
       + (reduce . map stAveChildren $ stChildrenList t)

{-# INLINABLE stMaxDepth #-}
stMaxDepth :: SpaceTree t dp => t dp -> Int
stMaxDepth t = if stHasNoChildren t
    then 1
    else 1+maximum (map stMaxDepth $ stChildrenList t)

{-# INLINABLE stNumSingletons #-}
stNumSingletons :: SpaceTree t dp => t dp -> Int
stNumSingletons t = if stHasNoChildren t
    then 0
    else sum (map stNumSingletons $ stChildrenList t) + if size (stChildrenList t) == 1
        then 1
        else 0

{-# INLINABLE stNumGhostSingletons #-}
stNumGhostSingletons :: SpaceTree t dp => t dp -> Int
stNumGhostSingletons t = if stHasNoChildren t
    then 0
    else sum (map stNumGhostSingletons $ stChildrenList t)
       + if size (stChildrenList t) == 1 && stWeight t==0
        then 1
        else 0

{-# INLINABLE stNumGhostLeaves #-}
stNumGhostLeaves :: SpaceTree t dp => t dp -> Int
stNumGhostLeaves t = if stHasNoChildren t
    then if stWeight t==0
        then 1
        else 0
    else sum (map stNumGhostLeaves $ stChildrenList t)

{-# INLINABLE stNumGhostSelfparent #-}
stNumGhostSelfparent :: (Eq dp, SpaceTree t dp) => t dp -> Int
stNumGhostSelfparent t = if stHasNoChildren t
    then 0
    else sum (map stNumGhostSelfparent $ stChildrenList t)
       + if stWeight t==0 && stNode t `elem` map stNode (stChildrenList t)
        then 1
        else 0

{-# INLINABLE stExtraLeaves #-}
stExtraLeaves :: (Eq dp, SpaceTree t dp) => t dp -> Int
stExtraLeaves t = if stHasNoChildren t
    then 0
    else sum (map stExtraLeaves $ stChildrenList t)
        + if supremum $ map (\c -> stNode c==stNode t && stHasNoChildren c) $ stChildrenList t
            then 1
            else 0

-------------------------------------------------------------------------------
-- pruning folds

---------------------------------------
-- single tree algs

{-# INLINABLE prunefoldinit #-}
prunefoldinit :: SpaceTree t dp => (t dp -> res) -> (res -> t dp -> Bool) -> (dp -> res -> res) -> t dp -> res
prunefoldinit init prune f t = foldl'
    (prunefold prune f)
    (init t)
    (stChildrenList t)

{-# INLINABLE prunefold #-}
prunefold :: SpaceTree t a => (b -> t a -> Bool) -> (a -> b -> b) -> b -> t a -> b
prunefold prune f b t = if prune b t
    then b
    else if stHasNoChildren t
        then b'
        else foldl' (prunefold prune f) b' (stChildrenList t)
    where
        b' = f (stNode t) b

{-# INLINABLE prunefoldW #-}
prunefoldW :: SpaceTree t a => (b -> t a -> Bool) -> (Weighted# a -> b -> b) -> b -> t a -> b
prunefoldW prune f b t = if prune b t
    then b
    else if stHasNoChildren t
        then b'
        else foldl' (prunefoldW prune f) b' (stChildrenList t)
    where
        b' = f (stNodeW t) b

{-# INLINABLE prunefoldA #-}
prunefoldA :: SpaceTree t a => (t a -> b -> Maybe' b) -> b -> t a -> b
prunefoldA !f !b !t = {-# SCC prunefoldA #-} case f t b of
    Nothing' -> b
    Just' b' -> foldl' (prunefoldA f) b' (stChildren t)

{-# INLINABLE prunefoldB #-}
prunefoldB :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Maybe' b) -> b -> t a -> b
prunefoldB !f1 !f2 !b !t = {-# SCC prunefoldB #-} case f2 t b of
    Nothing' -> {-# SCC prunefoldB_Nothing #-} b
    Just' b' -> {-# SCC prunefoldB_Just #-} foldl' (prunefoldB f1 f2) b'' (stChildren t)
        where
            b'' = {-# SCC b'' #-} foldr' f1 b' (stLeaves t)

-- {-# INLINABLE prunefoldB_CanError #-}
-- prunefoldB_CanError :: (SpaceTree t a, CanError b) =>
--     (a -> b -> b) -> (t a -> b -> b) -> b -> t a -> b
-- prunefoldB_CanError !f1 !f2 !b !t = go b t
--     where
--         go !b !t = {-# SCC prunefoldB_CanError #-} if isError res
--             then {-# SCC prunefoldB_CanError_Nothing #-} b
--             else {-# SCC prunefoldB_CanError_Just #-}
--                 ckfoldl' go b'' (stChildren t)
--                 where
--                     ckfoldl' a = {-# SCC ckfoldl #-} CK.foldl' a
--                     !res = f2 t b
--                     !b'' = {-# SCC b'' #-} ckfoldl' (flip f1) res (stLeaves t)

{-# INLINABLE prunefoldB_CanError #-}
-- {-# INLINE prunefoldB_CanError #-}
prunefoldB_CanError :: (SpaceTree t a, CanError b) =>
    (a -> b -> b) -> (t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_start #-} go t b
    where
        go !t !b = {-# SCC prunefoldB_CanError_if #-} if isError res
            then {-# SCC prunefoldB_CanError_Nothing #-} b
            else {-# SCC prunefoldB_CanError_Just #-}
                foldr' go b'' $ {-# SCC prunefoldB_CanError_stChildren #-} stChildren t
                where
                    res = {-# SCC res #-} f2 t b
                    b'' = {-# SCC b'' #-} foldr' f1 res (stLeaves t)

{-# INLINABLE prunefoldB_CanError_sort #-}
prunefoldB_CanError_sort :: (SpaceTree t a, CanError b) =>
    a -> (a -> b -> b) -> (Scalar a -> t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError_sort !query !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_sort #-}
    go ( distance (stNode t) query, t ) b
    where
        go !( dist,t ) !b =  if isError res
            then b
            else foldr' go b'' children' -- $ stChildren t
                where
                    res = f2 dist t b
                    b'' = foldr' f1 res (stLeaves t)

                    children'
                        = {-# SCC children' #-} qsortHalf (\( d1,_ ) ( d2,_ ) -> compare d2 d1)
                        $  map (\x -> ( distance (stNode x) query, x ))
                        $ toList
                        $ stChildren t

-- | This is a version of quicksort that only descends on its lower half.
-- That is, it only "approximately" sorts a list.
-- It is modified from http://en.literateprograms.org/Quicksort_%28Haskell%29
{-# INLINABLE qsortHalf #-}
qsortHalf :: (a -> a -> Ordering) -> [a] -> [a]
qsortHalf cmp x = go x []
    where
        go [] y     = y
        go [x] y    = x:y
        go (x:xs) y = part xs [] [x] []
            where
                part [] l e g = go l (e ++ g ++ y)
                part (z:zs) l e g = case cmp z x of
                    GT -> part zs l e (z:g)
                    LT -> part zs (z:l) e g
                    EQ -> part zs l (z:e) g

-- sortBy cmp = mergeAll . sequences
--   where
--     sequences (a:b:xs)
--       | a `cmp` b == GT = descending b [a]  xs
--       | otherwise       = ascending  b (a:) xs
--     sequences xs = [xs]
--
--     descending a as (b:bs)
--       | a `cmp` b == GT = descending b (a:as) bs
--     descending a as bs  = (a:as): sequences bs
--
--     ascending a as (b:bs)
--       | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
--     ascending a as bs   = as [a]: sequences bs
--
--     mergeAll [x] = x
--     mergeAll xs  = mergeAll (mergePairs xs)
--
--     mergePairs (a:b:xs) = merge a b: mergePairs xs
--     mergePairs xs       = xs
--
--     merge as@(a:as') bs@(b:bs')
--       | a `cmp` b == GT = b:merge as  bs'
--       | otherwise       = a:merge as' bs
--     merge [] bs         = bs
--     merge as []         = as

{-# INLINABLE prunefoldD #-}
-- {-# INLINE prunefoldD #-}
prunefoldD :: (SpaceTree t a, CanError b, Monoid b) =>
    (a -> b -> b) -> ( (# a,Scalar a #) -> b -> b) -> b -> t a -> b
prunefoldD !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_start #-} go t b
    where
        go !t !b =  if isError res
            then b
            else foldr' go b'' $ stChildren t
                where
                    !res = f2' t b
                    !b'' = foldr' f1 res (stLeaves t)

        f2' t b = f2 (# stNode t, lambda t #) b

--                     !b'' = foldr' f2' res (stLeaves t)

--         f2' dp b = if isError tmp
--             then b
--             else tmp
--             where tmp = f2 (singleton dp) b



{-# INLINABLE prunefoldC #-}
prunefoldC ::
    ( SpaceTree t a
    , CanError b
    ) => (a -> b -> b) -> b -> t a -> b
prunefoldC !f !b !t = go t b
    where
        go !t !b = if isError res
            then b
            else foldr' go b'' $ stChildren t
            where
                res = f (stNode t) b
                b'' = foldr' f1 res $ stLeaves t

        f1 a b = if isError a' then b else a'
            where a' = f a b

{-# INLINE noprune #-}
noprune :: b -> a -> Bool
noprune _ _ = False

