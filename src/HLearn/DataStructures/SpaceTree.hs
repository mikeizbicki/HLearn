{-# LANGUAGE DataKinds,MagicHash,UnboxedTuples #-}

module HLearn.DataStructures.SpaceTree
    (
    -- * Type classes
    SpaceTree (..)

    , Weighted#
    , DualTree (..)

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
--     , prunefoldC
--     , prunefoldM
    , noprune

    -- ** Dual tree
    , dualfold
    , prunefold2init
    , prunefold2
    , dualNodes
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

import HLearn.Algebra.Prim
import HLearn.Models.Distributions.Univariate.Normal

-------------------------------------------------------------------------------
-- SpaceTree

type Weighted# dp = (# Scalar dp, dp #)

class
    ( MetricSpace dp
    , Logic (t dp) ~ Bool
    , Logic dp ~ Bool
    , Eq_ (t dp)
    , Scalar dp ~ Scalar (t dp)
    , Elem (ChildContainer t (t dp)) ~ t dp
    , Elem (LeafContainer t dp) ~ dp
    , Unfoldable (LeafContainer t dp)
    , Unfoldable  (ChildContainer t (t dp))
    , Normed (LeafContainer t dp)
    , Normed (ChildContainer t (t dp))
    , Foldable (LeafContainer t dp)
    , Foldable (ChildContainer t (t dp))
    ) => SpaceTree t dp
        where

    type LeafContainer t :: * -> *
    type LeafContainer t = V.Vector

    type ChildContainer t :: * -> *
    type ChildContainer t = V.Vector

    {-# INLINE stMinDistance #-}
    {-# INLINE stMaxDistance #-}
    stMinDistance :: t dp -> t dp -> Scalar dp
    stMinDistance t1 t2 = fst# (stMinDistanceWithDistance t1 t2)
    stMaxDistance :: t dp -> t dp -> Scalar dp
    stMaxDistance t1 t2 = fst# (stMaxDistanceWithDistance t1 t2)

    stMinDistanceWithDistance :: t dp -> t dp -> (# Scalar dp, Scalar dp #)
    stMaxDistanceWithDistance :: t dp -> t dp -> (# Scalar dp, Scalar dp #)

    {-# INLINE stMinDistanceDp #-}
    {-# INLINE stMaxDistanceDp #-}
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

    {-# INLINE stHasNoChildren #-}
    stHasNoChildren :: t dp -> Bool
    stHasNoChildren t = isEmpty $ stChildren t

    {-# INLINE stChildrenList #-}
    stChildrenList :: t dp -> [t dp]
    stChildrenList t = toList $ stChildren t

    {-# INLINE stLeaves #-}
    stLeaves :: t dp -> LeafContainer t dp
    stLeaves t = zero

    {-# INLINE stNodeW #-}
    stNodeW :: t dp -> Weighted# dp
    stNodeW t = (# stWeight t, stNode t #)

    ro :: t dp -> Scalar dp
    lambda :: t dp -> Scalar dp

-------------------------------------------------------------------------------
-- generic algorithms

{-# INLINABLE stMaxDescendentDistance #-}
stMaxDescendentDistance :: (Eq dp, SpaceTree t dp) => t dp -> Scalar dp
stMaxDescendentDistance t = maximum_ 0 $ map (distance (stNode t)) $ stDescendents t

{-# INLINABLE stToListDFS #-}
stToListDFS :: SpaceTree t dp => t dp -> [dp]
stToListDFS t
    = stNode t
    : (toList $ stLeaves t)
    + (concat $ map stToListDFS $ stChildrenList t)

{-# INLINABLE stToList #-}
stToList :: SpaceTree t dp => t dp -> [dp]
stToList = stToListDFS

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
stNumLeaves t = (fromIntegral $ length (stLeaves t)) + sum (map stNumLeaves $ toList $ stChildren t)

{-# INLINABLE stNumGhosts #-}
stNumGhosts :: SpaceTree t dp => t dp -> Int
stNumGhosts t = (if stWeight t == 0 then 1 else 0) + if stHasNoChildren t
    then 0
    else sum (map stNumGhosts $ stChildrenList t)

{-# INLINABLE stAveGhostChildren #-}
stAveGhostChildren :: SpaceTree t dp => t dp -> Normal Double
stAveGhostChildren t =
    ( if stWeight t == 0
        then train1Normal . fromIntegral . length $ stChildrenList t
        else zero
    )
    +
    ( if stHasNoChildren t
        then zero
        else (reduce . map stAveGhostChildren $ stChildrenList t)
    )

{-# INLINABLE stMaxLeaves #-}
stMaxLeaves :: SpaceTree t dp => t dp -> Int
stMaxLeaves t = maximum $ (length $ toList $ stLeaves t):(map stMaxLeaves $ stChildrenList t)

{-# INLINABLE stAveLeaves #-}
stAveLeaves :: SpaceTree t dp => t dp -> Normal Double
stAveLeaves t = (train1Normal . fromIntegral . length . toList $ stLeaves t)
              + (reduce . map stAveLeaves $ stChildrenList t)

{-# INLINABLE stMaxChildren #-}
stMaxChildren :: SpaceTree t dp => t dp -> Int
stMaxChildren t = if stHasNoChildren t
    then 0
    else maximum $ (length $ stChildrenList t):(map stMaxChildren $ stChildrenList t)

{-# INLINABLE stAveChildren #-}
stAveChildren :: SpaceTree t dp => t dp -> Normal Double
stAveChildren t = if stHasNoChildren t
    then zero
    else (train1Normal . fromIntegral . length $ stChildrenList t)
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
    else sum (map stNumSingletons $ stChildrenList t) + if length (stChildrenList t) == 1
        then 1
        else 0

{-# INLINABLE stNumGhostSingletons #-}
stNumGhostSingletons :: SpaceTree t dp => t dp -> Int
stNumGhostSingletons t = if stHasNoChildren t
    then 0
    else sum (map stNumGhostSingletons $ stChildrenList t)
       + if length (stChildrenList t) == 1 && stWeight t==0
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
-- prunefoldB_CanError :: (VG.Vector (ChildContainer t) (t a), VG.Vector (LeafContainer t) a, SpaceTree t a, CanError b) =>
prunefoldB_CanError :: (SpaceTree t a, CanError b) =>
    (a -> b -> b) -> (t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_start #-} go t b
    where
        go !t !b = {-# SCC prunefoldB_CanError_if #-} if isError res
            then {-# SCC prunefoldB_CanError_Nothing #-} b
            else {-# SCC prunefoldB_CanError_Just #-}
                foldr' go b'' $ {-# SCC prunefoldB_CanError_stChildren #-} stChildren t
                where
                    !res = {-# SCC res #-} f2 t b
                    !b'' = {-# SCC b'' #-} foldr' f1 res (stLeaves t)

{-# INLINE vecfold #-}
-- vecfold :: VG.Vector v a => (a -> b -> a) -> b -> v a -> b
vecfold !f !tot !v = {-# SCC vecfold #-} if VG.length v > 0
    then goEach 0 tot
    else tot
    where
        goEach !i !tot = if i>=VG.length v
            then tot
            else goEach (i+1) $ f (v `VG.unsafeIndex` i) tot

-- {-# INLINABLE prunefoldC #-}
-- prunefoldC :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Strict.Either b b) -> b -> t a -> b
-- prunefoldC !f1 !f2 !b !t = case f2 t b of
--     Strict.Left b' -> b'
--     Strict.Right b' -> ckfoldl' (prunefoldC f1 f2) b'' (stChildren t)
--         where
--             b'' = CK.foldr' f1 b' (stLeaves t)

{-# INLINE noprune #-}
noprune :: b -> a -> Bool
noprune _ _ = False

---------------------------------------
-- dual tree algs

data DualTree a = DualTree
    { reference :: !a
    , query     :: !a
    }
    deriving (Read,Show)

instance (Semigroup a,Eq a) => Semigroup (DualTree a) where
    a+b = if reference a==reference b
        then DualTree
            { reference = reference a
            , query = query a + query b
            }
        else error "DualTree Monoid requires both references to be equal"

instance (Monoid a,Eq a) => Monoid (DualTree a) where
    zero = DualTree
        { reference = undefined
        , query = zero
        }


-- instance Cocommutative a => Cocommutative (DualTree a)
-- instance NonCocommutative a => NonCocommutative (DualTree a)
-- instance Comonoid a => Comonoid (DualTree a) where
--     partition n dual = [DualTree (reference dual) q | q <- partition n $ query dual]

{-# INLINABLE dualNodes #-}
dualNodes :: SpaceTree t dp => DualTree (t dp) -> DualTree dp
dualNodes dual = DualTree (stNode $ reference dual) (stNode $ query dual)

{-# INLINABLE prunefold2init #-}
prunefold2init ::
    ( SpaceTree t dp
    ) => (DualTree (t dp) -> res)
    -> (res -> DualTree (t dp) -> Bool)
    -> (DualTree dp -> res -> res)
    -> DualTree (t dp)
    -> res
prunefold2init init prune f pair = foldl'
    (prunefold2 prune f)
    (init pair)
    (dualTreeMatrix (stChildrenList $ reference pair) (stChildrenList $ query pair))

{-# INLINABLE dualfold #-}
dualfold ::
    ( SpaceTree t dp
    )
    => (DualTree (t dp) -> res -> res)
    -> (res -> DualTree (t dp) -> Bool)
    -> (DualTree dp -> res -> res)
    -> res
    -> DualTree (t dp)
    -> res
dualfold tag prune f b pair = if prune b_tagged pair
    then b_tagged
    else if stHasNoChildren (reference pair) && stHasNoChildren (query pair)
        then b'
        else foldl'
            (dualfold tag prune f)
            b'
            (dualTreeMatrix (stChildrenList $ reference pair) (stChildrenList $ query pair))
    where
        b_tagged = tag pair b
        b' = f (dualNodes pair) b_tagged

{-# INLINABLE prunefold2 #-}
prunefold2 ::
    ( SpaceTree t dp
    ) =>(res -> DualTree (t dp) -> Bool)
    -> (DualTree dp -> res -> res)
    -> res
    -> DualTree (t dp)
    -> res
prunefold2 prune f b pair = if prune b pair
    then b
    else if stHasNoChildren (reference pair) && stHasNoChildren (query pair)
        then b'
        else foldl'
            (prunefold2 prune f)
            b'
            (dualTreeMatrix (stChildrenList $ reference pair) (stChildrenList $ query pair))
    where
        b' = f (dualNodes pair) b

dualTreeMatrix :: [a] -> [a] -> [DualTree a]
dualTreeMatrix xs [] = []
dualTreeMatrix xs (y:ys) = fmap (\x -> DualTree x y) xs ++ dualTreeMatrix xs ys

