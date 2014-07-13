{-# LANGUAGE DataKinds,MagicHash,UnboxedTuples #-}

module HLearn.DataStructures.SpaceTree
    ( 
    -- * Type classes
    SpaceTree (..)
    , Taggable (..)

    , AddUnit (..)
    , DualTree (..)

--     , Container
    , FromList (..)

    -- * Generic algorithms
    , stToList
    , stToListW
    , stDescendents
    , stNumDp
    , stNumNodes
    , stNumLeaves
    , stNumGhosts
    , stAveGhostChildren
    , stNumGhostSingletons
    , stNumGhostLeaves
    , stNumGhostSelfparent
    , stMaxChildren
    , stAveChildren
    , stMaxDepth
    , stNumSingletons
    , stExtraLeaves
    , toTagList

    -- * Pruning folds

    -- ** Single tree
    
    , prunefoldinit
    , prunefold
    , prunefoldA
    , prunefoldB
    , prunefoldB_CanError
    , prunefoldC
--     , prunefoldM
    , noprune

    -- ** Dual tree
    , dualfold
    , prunefold2init
    , prunefold2
    , dualNodes
    )
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Semigroup
import Data.List hiding (partition)
import Data.Traversable
import qualified Data.Foldable as F
import qualified Data.Strict.Either as Strict
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import Unsafe.Coerce
import GHC.Prim

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra hiding ((<>))
import HLearn.Algebra.Prim
import HLearn.Models.Distributions
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictVector as Strict

-------------------------------------------------------------------------------
-- SpaceTree 

class Taggable t dp where

    getTag :: t tag dp -> tag

    nodeIndex :: t (Int,Int) dp -> Int
    nodeIndex = fst . getTag

    dpIndex :: t (Int,Int) dp -> Int
    dpIndex = snd . getTag

    initTags :: t () dp -> (t (Int,Int) dp,Int,Int)
    clearTags :: t tag dp -> t () dp


class 
    ( CK.Functor f
    , CK.FunctorConstraint f a
    , CK.Foldable f
    , CK.FoldableConstraint f a
    ) => FromList f a 
        where
--     ckfmap :: (a -> b) -> f a -> f b
    ckfoldl' :: (b -> a -> b) -> b -> f a -> b
    fromList :: [a] -> f a
    toList :: f a -> [a]

instance FromList V.Vector a where
--     {-# INLINE ckfmap #-}
    {-# INLINE ckfoldl' #-}
--     ckfmap = V.map
    ckfoldl' = V.foldl'
    {-# INLINE fromList #-}
    {-# INLINE toList #-}
    fromList = VG.fromList
    toList = VG.toList


instance VU.Unbox a => FromList VU.Vector a where
--     {-# INLINE ckfmap #-}
    {-# INLINE ckfoldl' #-}
--     ckfmap = VU.map
    ckfoldl' = VU.foldl'
    {-# INLINE fromList #-}
    {-# INLINE toList #-}
    fromList = VG.fromList
    toList = VG.toList

-- instance FromList StrictVector a where
--     {-# INLINE fromList #-}
--     {-# INLINE toList #-}
--     fromList = VG.fromList
--     toList = VG.toList

instance FromList [] a where
--     {-# INLINE ckfmap #-}
    {-# INLINE ckfoldl' #-}
--     ckfmap = map
    ckfoldl' = foldl'
    {-# INLINE fromList #-}
    {-# INLINE toList #-}
    fromList = id
    toList = id

-- instance CK.Functor Strict.List where
--     fmap = fmap
-- 
-- instance CK.Foldable Strict.List where
--     foldl = F.foldl
--     foldl' = F.foldl'
--     foldl1 = F.foldl1
--     foldr = F.foldr
--     foldr' = F.foldr'
--     foldr1 = F.foldr1
-- 
-- instance FromList Strict.List a where
--     {-# INLINE fromList #-}
--     {-# INLINE toList #-}
--     fromList [] = Strict.Nil
--     fromList (x:xs) = x Strict.:. fromList xs
-- 
--     toList = Strict.strictlist2list

-- class (Functor a, F.Foldable a, FromList a) => Container a
-- instance (Functor a, F.Foldable a, FromList a) => Container a

class 
    ( MetricSpace dp
    , Scalar dp ~ Scalar (t dp)
    , NumDP (t dp)
    , FromList (NodeContainer t) dp
    , FromList (ChildContainer t) dp
    , FromList (ChildContainer t) (t dp)
    , Monoid (NodeContainer t dp)
    , VG.Vector (ChildContainer t) (t dp)
    , VG.Vector (NodeContainer t) dp
    ) => SpaceTree t dp 
        where
    type NodeContainer t :: * -> *
    type NodeContainer t = V.Vector 

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

    stIsMinDistanceDpFartherThanWithDistance :: t dp -> dp -> Scalar dp -> Strict.Maybe (Scalar dp)
    stIsMaxDistanceDpFartherThanWithDistance :: t dp -> dp -> Scalar dp -> Strict.Maybe (Scalar dp)

    stIsMinDistanceDpFartherThanWithDistanceCanError :: CanError (Scalar dp) => t dp -> dp -> Scalar dp -> Scalar dp
    stIsMaxDistanceDpFartherThanWithDistanceCanError :: CanError (Scalar dp) => t dp -> dp -> Scalar dp -> Scalar dp

    stMinDistanceDpWithDistance :: t dp -> dp -> (# Scalar dp, Scalar dp #)
    stMaxDistanceDpWithDistance :: t dp -> dp -> (# Scalar dp, Scalar dp #)

    stMinDistanceDpFromDistance :: t dp -> dp -> Scalar dp -> Scalar dp
    stMaxDistanceDpFromDistance :: t dp -> dp -> Scalar dp -> Scalar dp

    stHasNode   :: t dp -> Bool
    stIsLeaf    :: t dp -> Bool
    stChildren  :: t dp -> (ChildContainer t) (t dp)
    stNode      :: t dp -> dp
    stWeight    :: t dp -> Scalar dp

    {-# INLINE stChildrenList #-}
    stChildrenList  :: t dp -> [t dp]
    stChildrenList t = toList $ stChildren t

    {-# INLINE stNodeV #-}
    stNodeV :: t dp -> (NodeContainer t) dp
    stNodeV t = mempty
    
    {-# INLINE stNodeW #-}
    stNodeW :: t dp -> Weighted dp
    stNodeW t = (stWeight t, stNode t)

    ro :: t dp -> Scalar dp
    lambda :: t dp -> Scalar dp

-------------------------------------------------------------------------------
-- generic algorithms

{-# INLINABLE stToList #-}
stToList :: (Eq dp, SpaceTree t dp) => t dp -> [dp]
stToList t = if stIsLeaf t && stWeight t > 0
    then (stNode t):(toList $ stNodeV t)
    else go (concat $ map stToList $ stChildrenList t)
    where
        go xs = if stWeight t > 0 
            then (stNode t) : (toList (stNodeV t) ++ xs)
            else toList (stNodeV t) ++ xs
    
{-# INLINABLE stToListW #-}
stToListW :: (Eq dp, SpaceTree t dp) => t dp -> [Weighted dp]
stToListW t = if stIsLeaf t && stWeight t > 0
    then [(stWeight t,stNode t)]
    else go (concat $ map stToListW $ stChildrenList t)
    where
        go xs = if stWeight t > 0 
            then (stWeight t,stNode t) : xs
            else xs

{-# INLINABLE toTagList #-}
toTagList :: (Eq dp, SpaceTree (t tag) dp, Taggable t dp) => t tag dp -> [(dp,tag)]
toTagList t = if stIsLeaf t
    then [(stNode t,getTag t)]
    else go (concat $ map toTagList $ stChildrenList t)
    where 
        go xs = if stNode t `Data.List.elem` (map stNode $ stChildrenList t)
            then xs
            else (stNode t,getTag t) : xs

{-# INLINABLE stDescendents #-}
stDescendents :: SpaceTree t dp => t dp -> [dp]
stDescendents t = if stIsLeaf t 
    then [stNode t]
    else concatMap stDescendents $ stChildrenList t

{-# INLINABLE stNumDp #-}
stNumDp :: SpaceTree t dp => t dp -> Scalar dp
stNumDp t = if stIsLeaf t
    then stWeight t
    else stWeight t + sum (map stNumDp $ stChildrenList t)

{-# INLINABLE stNumNodes #-}
stNumNodes :: SpaceTree t dp => t dp -> Int
stNumNodes t = if stIsLeaf t
    then 1
    else 1 + sum (map stNumNodes $ stChildrenList t)

{-# INLINABLE stNumLeaves #-}
stNumLeaves :: SpaceTree t dp => t dp -> Int
stNumLeaves t = if stIsLeaf t
    then 1
    else sum (map stNumLeaves $ stChildrenList t)

{-# INLINABLE stNumGhosts #-}
stNumGhosts :: SpaceTree t dp => t dp -> Int
stNumGhosts t = (if stWeight t == 0 then 1 else 0) + if stIsLeaf t
    then 0
    else sum (map stNumGhosts $ stChildrenList t)

{-# INLINABLE stAveGhostChildren #-}
stAveGhostChildren :: SpaceTree t dp => t dp -> Normal Double Double
stAveGhostChildren t = (if stWeight t == 0 then train1dp . fromIntegral . length $ stChildrenList t else mempty)
    `mappend` if stIsLeaf t
        then mempty
        else (reduce . map stAveGhostChildren $ stChildrenList t)

{-# INLINABLE stMaxChildren #-}
stMaxChildren :: SpaceTree t dp => t dp -> Int
stMaxChildren t = if stIsLeaf t
    then 0
    else maximum $ (length $ stChildrenList t):(map stMaxChildren $ stChildrenList t)

{-# INLINABLE stAveChildren #-}
stAveChildren :: SpaceTree t dp => t dp -> Normal Double Double
stAveChildren t = if stIsLeaf t
    then mempty
    else (train1dp . fromIntegral . length $ stChildrenList t) `mappend` (reduce . map stAveChildren $ stChildrenList t)

{-# INLINABLE stMaxDepth #-}
stMaxDepth :: SpaceTree t dp => t dp -> Int
stMaxDepth t = if stIsLeaf t
    then 1
    else 1+maximum (map stMaxDepth $ stChildrenList t)

{-# INLINABLE stNumSingletons #-}
stNumSingletons :: SpaceTree t dp => t dp -> Int
stNumSingletons t = if stIsLeaf t
    then 0
    else sum (map stNumSingletons $ stChildrenList t) + if length (stChildrenList t) == 1
        then 1
        else 0 

{-# INLINABLE stNumGhostSingletons #-}
stNumGhostSingletons :: SpaceTree t dp => t dp -> Int
stNumGhostSingletons t = if stIsLeaf t
    then 0
    else sum (map stNumGhostSingletons $ stChildrenList t) + if length (stChildrenList t) == 1 && stWeight t==0
        then 1
        else 0 

{-# INLINABLE stNumGhostLeaves #-}
stNumGhostLeaves :: SpaceTree t dp => t dp -> Int
stNumGhostLeaves t = if stIsLeaf t
    then if stWeight t==0
        then 1
        else 0
    else sum (map stNumGhostLeaves $ stChildrenList t)

{-# INLINABLE stNumGhostSelfparent #-}
stNumGhostSelfparent :: (Eq dp, SpaceTree t dp) => t dp -> Int
stNumGhostSelfparent t = if stIsLeaf t 
    then 0
    else sum (map stNumGhostSelfparent $ stChildrenList t) 
       + if stWeight t==0 && stNode t `Data.List.elem` map stNode (stChildrenList t)
        then 1
        else 0

{-# INLINABLE stExtraLeaves #-}
stExtraLeaves :: (Eq dp, SpaceTree t dp) => t dp -> Int
stExtraLeaves t = if stIsLeaf t
    then 0
    else sum (map stExtraLeaves $ stChildrenList t) 
        + if or $ map (\c -> stNode c==stNode t && stIsLeaf c) $ stChildrenList t
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
    else if stIsLeaf t
        then b'
        else foldl' (prunefold prune f) b' (stChildrenList t) 
    where
        b' = f (stNode t) b

{-# INLINABLE prunefoldW #-}
prunefoldW :: SpaceTree t a => (b -> t a -> Bool) -> (Weighted a -> b -> b) -> b -> t a -> b
prunefoldW prune f b t = if prune b t
    then b
    else if stIsLeaf t
        then b'
        else foldl' (prunefoldW prune f) b' (stChildrenList t) 
    where
        b' = f (stNodeW t) b

{-# INLINABLE prunefoldA #-}
prunefoldA :: SpaceTree t a => (t a -> b -> Strict.Maybe b) -> b -> t a -> b
prunefoldA !f !b !t = {-# SCC prunefoldA #-} case f t b of
    Strict.Nothing -> b
    Strict.Just b' -> CK.foldl' (prunefoldA f) b' (stChildren t)

{-# INLINABLE prunefoldB #-}
prunefoldB :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Strict.Maybe b) -> b -> t a -> b
prunefoldB !f1 !f2 !b !t = {-# SCC prunefoldB #-} case f2 t b of
    Strict.Nothing -> {-# SCC prunefoldB_Nothing #-} b
    Strict.Just b' -> {-# SCC prunefoldB_Just #-} CK.foldl' (prunefoldB f1 f2) b'' (stChildren t)
        where
            b'' = {-# SCC b'' #-} CK.foldr' f1 b' (stNodeV t)

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
--                     !b'' = {-# SCC b'' #-} ckfoldl' (flip f1) res (stNodeV t)

{-# INLINABLE prunefoldB_CanError #-}
prunefoldB_CanError :: (VG.Vector (ChildContainer t) (t a), VG.Vector (NodeContainer t) a, SpaceTree t a, CanError b) =>
    (a -> b -> b) -> (t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_start #-} go t b
    where
        go !t !b = {-# SCC prunefoldB_CanError_if #-} if isError res
            then {-# SCC prunefoldB_CanError_Nothing #-} b
            else {-# SCC prunefoldB_CanError_Just #-} 
                vecfold go b'' ({-# SCC prunefoldB_CanError_stChildren #-} stChildren t)
                where
                    !res = {-# SCC res #-} f2 t b
                    !b'' = {-# SCC b'' #-} vecfold f1 res (stNodeV t)

{-# INLINE vecfold #-}
-- vecfold :: VG.Vector v a => (a -> b -> a) -> b -> v a -> b
vecfold !f !tot !v = {-# SCC vecfold #-} go 0 tot
    where
        go !i !tot = if i>=VG.length v
            then tot
            else go (i+1) $ f (v `VG.unsafeIndex` i) tot

{-# INLINABLE prunefoldC #-}
prunefoldC :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Strict.Either b b) -> b -> t a -> b
prunefoldC !f1 !f2 !b !t = case f2 t b of
    Strict.Left b' -> b'
    Strict.Right b' -> ckfoldl' (prunefoldC f1 f2) b'' (stChildren t)
        where
            b'' = CK.foldr' f1 b' (stNodeV t)

{-# INLINE noprune #-}
noprune :: b -> a -> Bool
noprune _ _ = False

---------------------------------------
-- dual tree algs

data DualTree a = DualTree 
    { reference :: !a
    , query     :: !a
    }
    deriving (Read,Show,Eq,Ord)

instance (Monoid a,Eq a) => Monoid (DualTree a) where
    mempty = DualTree 
        { reference = undefined
        , query = mempty
        }

    mappend a b = if reference a==reference b
        then DualTree
            { reference = reference a
            , query = query a `mappend` query b
            }
        else error "DualTree Monoid requires both references to be equal"

instance Cocommutative a => Cocommutative (DualTree a)
instance NonCocommutative a => NonCocommutative (DualTree a)
instance Comonoid a => Comonoid (DualTree a) where
    partition n dual = [DualTree (reference dual) q | q <- partition n $ query dual]

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
    else if stIsLeaf (reference pair) && stIsLeaf (query pair)
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
    else if stIsLeaf (reference pair) && stIsLeaf (query pair)
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

-------------------------------------------------------------------------------
-- AddUnit

data AddUnit sg tag dp
    = Unit 
    | UnitLift { unUnit :: !(sg tag dp) }
    deriving (Read,Show,Eq,Ord)

instance NFData (sg tag dp) => NFData (AddUnit sg tag dp) where
    {-# INLINE rnf #-}
    rnf Unit = ()
    rnf (UnitLift sg) = rnf sg

type instance Scalar (AddUnit sg tag dp) = Scalar (sg tag dp)

instance Semigroup (sg tag dp) => Monoid (AddUnit sg tag dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    mempty = Unit
    mappend Unit x = x
    mappend x Unit = x
    mappend (UnitLift x) (UnitLift y) = UnitLift $ x<>y
 
instance F.Foldable (sg tag) => F.Foldable (AddUnit sg tag) where
    {-# INLINE foldr #-}
    foldr f i Unit = error "foldr Unit"
    foldr f i (UnitLift x) = F.foldr f i x

instance Taggable sg dp => Taggable (AddUnit sg) dp where
    {-# INLINE getTag #-}
    {-# INLINE initTags #-}
    {-# INLINE clearTags #-}

--     getTag Unit = Nothing
    getTag (UnitLift sg) = getTag sg

    initTags Unit = (Unit,0,0)
    initTags (UnitLift sg) = (UnitLift a,b,c)
        where
            (a,b,c) = initTags sg

    clearTags Unit = Unit
    clearTags (UnitLift sg) = UnitLift $ clearTags sg

instance (Num (Scalar (sg tag dp)), NumDP (sg tag dp)) => NumDP (AddUnit sg tag dp) where
    numdp Unit = 0
    numdp (UnitLift x) = numdp x

instance 
    ( SpaceTree (sg tag) dp
    , FromList (ChildContainer (sg tag)) (AddUnit sg tag dp)
    , VG.Vector (ChildContainer (sg tag)) (AddUnit sg tag dp)
    , VG.Vector (ChildContainer (sg tag)) dp
    , Monoid ((ChildContainer (sg tag)) (AddUnit sg tag dp))
    , Functor (ChildContainer (sg tag))
    , NumDP (sg tag dp)
    ) => SpaceTree (AddUnit sg tag) dp 
        where
    type NodeContainer (AddUnit sg tag) = NodeContainer (sg tag)
    type ChildContainer (AddUnit sg tag) = ChildContainer (sg tag)

    {-# INLINE stMinDistance #-}
    {-# INLINE stMaxDistance #-}
    {-# INLINE stMinDistanceWithDistance #-}
    {-# INLINE stMaxDistanceWithDistance #-}
    {-# INLINE stMinDistanceDp #-}
    {-# INLINE stMaxDistanceDp #-}
    {-# INLINE stMinDistanceDpWithDistance #-}
    {-# INLINE stMaxDistanceDpWithDistance #-}
    {-# INLINE stIsMinDistanceDpFartherThanWithDistance #-}
    {-# INLINE stIsMaxDistanceDpFartherThanWithDistance #-}
    {-# INLINE stChildrenList #-}
    {-# INLINE stChildren #-}
    {-# INLINE stNode #-}
    {-# INLINE stHasNode #-}
    {-# INLINE stIsLeaf #-}

    stMinDistance Unit x = 0
    stMinDistance x Unit = 0
    stMinDistance (UnitLift x) (UnitLift y) = stMinDistance x y

    stMaxDistance Unit x = infinity
    stMaxDistance x Unit = infinity
    stMaxDistance (UnitLift x) (UnitLift y) = stMaxDistance x y

    stMinDistanceWithDistance (UnitLift x) (UnitLift y) = stMinDistanceWithDistance x y
    stMaxDistanceWithDistance (UnitLift x) (UnitLift y) = stMaxDistanceWithDistance x y

    stMinDistanceDp Unit dp = 0
    stMinDistanceDp (UnitLift x) dp = stMinDistanceDp x dp
    stMaxDistanceDp Unit dp = infinity
    stMaxDistanceDp (UnitLift x) dp = stMaxDistanceDp x dp

    stMinDistanceDpWithDistance Unit dp = (# 0, 0 #)
    stMinDistanceDpWithDistance (UnitLift x) dp = stMinDistanceDpWithDistance x dp
    stMaxDistanceDpWithDistance Unit dp = (# infinity, infinity #)
    stMaxDistanceDpWithDistance (UnitLift x) dp = stMaxDistanceDpWithDistance x dp

    stIsMinDistanceDpFartherThanWithDistance (UnitLift x) dp b = stIsMinDistanceDpFartherThanWithDistance x dp b
    stIsMaxDistanceDpFartherThanWithDistance (UnitLift x) dp b = stIsMaxDistanceDpFartherThanWithDistance x dp b

    {-# INLINE stIsMinDistanceDpFartherThanWithDistanceCanError #-}
    stIsMinDistanceDpFartherThanWithDistanceCanError Unit dp b = errorVal 
    stIsMinDistanceDpFartherThanWithDistanceCanError (UnitLift x) dp b = 
        stIsMinDistanceDpFartherThanWithDistanceCanError x dp b

    {-# INLINE stIsMaxDistanceDpFartherThanWithDistanceCanError #-}
    stIsMaxDistanceDpFartherThanWithDistanceCanError Unit dp b = errorVal 
    stIsMaxDistanceDpFartherThanWithDistanceCanError (UnitLift x) dp b = 
        stIsMaxDistanceDpFartherThanWithDistanceCanError x dp b

    stMinDistanceDpFromDistance Unit _ _ = 0
    stMinDistanceDpFromDistance (UnitLift x) dp dist = stMinDistanceDpFromDistance x dp dist
    stMaxDistanceDpFromDistance Unit _ _ = infinity
    stMaxDistanceDpFromDistance (UnitLift x) dp dist = stMaxDistanceDpFromDistance x dp dist

    stChildrenList Unit = []
    stChildrenList (UnitLift x) = map UnitLift $ stChildrenList x

    stChildren Unit = mempty
    stChildren (UnitLift x) = fmap UnitLift $ stChildren x

    stNode Unit = error "stNode Unit"
    stNode (UnitLift x) = stNode x

    stWeight Unit = 0
    stWeight (UnitLift x) = stWeight x

    stHasNode Unit = False
    stHasNode (UnitLift x) = stHasNode x

    stIsLeaf Unit = False
    stIsLeaf (UnitLift x) = stIsLeaf x 

    ro Unit = error "ro Unit"
    ro (UnitLift x) = ro x

    lambda Unit = error "lambda Unit"
    lambda (UnitLift x) = lambda x
