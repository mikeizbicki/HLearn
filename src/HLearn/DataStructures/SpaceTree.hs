{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree
    ( 
    -- * Type classes
    SpaceTree (..)
    , Taggable (..)

    , AddUnit (..)
    , DualTree (..)

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
import qualified Data.Foldable as F
import qualified Data.Strict.Either as Strict
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import HLearn.Algebra hiding ((<>))
import HLearn.Models.Distributions
import qualified HLearn.DataStructures.StrictList as Strict

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
    ( MetricSpace dp
    , Ring dp ~ Ring (t dp)
    , NumDP (t dp)
    , VG.Vector (LeafVector t) dp
    ) => SpaceTree t dp 
        where
    type LeafVector t :: * -> *
    type LeafVector t = V.Vector 

    {-# INLINE stMinDistance #-}
    {-# INLINE stMaxDistance #-}
    stMinDistance :: t dp -> t dp -> Ring dp
    stMinDistance t1 t2 = Strict.fst $ stMinDistanceWithDistance t1 t2
    stMaxDistance :: t dp -> t dp -> Ring dp
    stMaxDistance t1 t2 = Strict.fst $ stMaxDistanceWithDistance t1 t2
    
    stMinDistanceWithDistance :: t dp -> t dp -> Strict.Pair (Ring dp) (Ring dp)
    stMaxDistanceWithDistance :: t dp -> t dp -> Strict.Pair (Ring dp) (Ring dp)

    {-# INLINE stMinDistanceDp #-}
    {-# INLINE stMaxDistanceDp #-}
    stMinDistanceDp :: t dp -> dp -> Ring dp
    stMinDistanceDp t dp = Strict.fst $ stMinDistanceDpWithDistance t dp
    stMaxDistanceDp :: t dp -> dp -> Ring dp
    stMaxDistanceDp t dp = Strict.fst $ stMaxDistanceDpWithDistance t dp

    stIsMinDistanceDpFartherThanWithDistance :: t dp -> dp -> Ring dp -> Strict.Maybe (Ring dp)
    stIsMaxDistanceDpFartherThanWithDistance :: t dp -> dp -> Ring dp -> Strict.Maybe (Ring dp)

    stMinDistanceDpWithDistance :: t dp -> dp -> Strict.Pair (Ring dp) (Ring dp)
    stMaxDistanceDpWithDistance :: t dp -> dp -> Strict.Pair (Ring dp) (Ring dp)

    stMinDistanceDpFromDistance :: t dp -> dp -> Ring dp -> Ring dp
    stMaxDistanceDpFromDistance :: t dp -> dp -> Ring dp -> Ring dp

    stHasNode   :: t dp -> Bool
    stIsLeaf    :: t dp -> Bool
    stChildren  :: t dp -> [t dp]
    stChildren' :: t dp -> Strict.List (t dp)
    stChildren_ :: t dp -> V.Vector (t dp)
    stNode      :: t dp -> dp
    stWeight    :: t dp -> Ring dp

    {-# INLINE stNodeV #-}
    stNodeV :: t dp -> (LeafVector t) dp
    stNodeV t = VG.empty
    
    {-# INLINE stNodeW #-}
    stNodeW :: t dp -> Weighted dp
    stNodeW t = (stWeight t, stNode t)

    ro :: t dp -> Ring dp
    lambda :: t dp -> Ring dp

-------------------------------------------------------------------------------
-- generic algorithms

{-# INLINABLE stToList #-}
stToList :: (Eq dp, SpaceTree t dp) => t dp -> [dp]
stToList t = if stIsLeaf t && stWeight t > 0
    then [stNode t]
    else go (concat $ map stToList $ stChildren t)
    where
        go xs = if stWeight t > 0 
            then (stNode t) : xs
            else xs
    
{-# INLINABLE stToListW #-}
stToListW :: (Eq dp, SpaceTree t dp) => t dp -> [Weighted dp]
stToListW t = if stIsLeaf t && stWeight t > 0
    then [(stWeight t,stNode t)]
    else go (concat $ map stToListW $ stChildren t)
    where
        go xs = if stWeight t > 0 
            then (stWeight t,stNode t) : xs
            else xs

{-# INLINABLE toTagList #-}
toTagList :: (Eq dp, SpaceTree (t tag) dp, Taggable t dp) => t tag dp -> [(dp,tag)]
toTagList t = if stIsLeaf t
    then [(stNode t,getTag t)]
    else go (concat $ map toTagList $ stChildren t)
    where 
        go xs = if stNode t `Data.List.elem` (map stNode $ stChildren t)
            then xs
            else (stNode t,getTag t) : xs

{-# INLINABLE stDescendents #-}
stDescendents :: SpaceTree t dp => t dp -> [dp]
stDescendents t = if stIsLeaf t 
    then [stNode t]
    else concatMap stDescendents $ stChildren t

{-# INLINABLE stNumDp #-}
stNumDp :: SpaceTree t dp => t dp -> Ring dp
stNumDp t = if stIsLeaf t
    then stWeight t
    else stWeight t + sum (map stNumDp $ stChildren t)

{-# INLINABLE stNumNodes #-}
stNumNodes :: SpaceTree t dp => t dp -> Int
stNumNodes t = if stIsLeaf t
    then 1
    else 1 + sum (map stNumNodes $ stChildren t)

{-# INLINABLE stNumLeaves #-}
stNumLeaves :: SpaceTree t dp => t dp -> Int
stNumLeaves t = if stIsLeaf t
    then 1
    else sum (map stNumLeaves $ stChildren t)

{-# INLINABLE stNumGhosts #-}
stNumGhosts :: SpaceTree t dp => t dp -> Int
stNumGhosts t = (if stWeight t == 0 then 1 else 0) + if stIsLeaf t
    then 0
    else sum (map stNumGhosts $ stChildren t)

{-# INLINABLE stAveGhostChildren #-}
stAveGhostChildren :: SpaceTree t dp => t dp -> Normal Double Double
stAveGhostChildren t = (if stWeight t == 0 then train1dp . fromIntegral . length $ stChildren t else mempty)
    `mappend` if stIsLeaf t
        then mempty
        else (reduce . map stAveGhostChildren $ stChildren t)

{-# INLINABLE stMaxChildren #-}
stMaxChildren :: SpaceTree t dp => t dp -> Int
stMaxChildren t = if stIsLeaf t
    then 0
    else maximum $ (length $ stChildren t):(map stMaxChildren $ stChildren t)

{-# INLINABLE stAveChildren #-}
stAveChildren :: SpaceTree t dp => t dp -> Normal Double Double
stAveChildren t = if stIsLeaf t
    then mempty
    else (train1dp . fromIntegral . length $ stChildren t) `mappend` (reduce . map stAveChildren $ stChildren t)

{-# INLINABLE stMaxDepth #-}
stMaxDepth :: SpaceTree t dp => t dp -> Int
stMaxDepth t = if stIsLeaf t
    then 1
    else 1+maximum (map stMaxDepth $ stChildren t)

{-# INLINABLE stNumSingletons #-}
stNumSingletons :: SpaceTree t dp => t dp -> Int
stNumSingletons t = if stIsLeaf t
    then 0
    else sum (map stNumSingletons $ stChildren t) + if length (stChildren t) == 1
        then 1
        else 0 

{-# INLINABLE stNumGhostSingletons #-}
stNumGhostSingletons :: SpaceTree t dp => t dp -> Int
stNumGhostSingletons t = if stIsLeaf t
    then 0
    else sum (map stNumGhostSingletons $ stChildren t) + if length (stChildren t) == 1 && stWeight t==0
        then 1
        else 0 

{-# INLINABLE stNumGhostLeaves #-}
stNumGhostLeaves :: SpaceTree t dp => t dp -> Int
stNumGhostLeaves t = if stIsLeaf t
    then if stWeight t==0
        then 1
        else 0
    else sum (map stNumGhostLeaves $ stChildren t)

{-# INLINABLE stNumGhostSelfparent #-}
stNumGhostSelfparent :: (Eq dp, SpaceTree t dp) => t dp -> Int
stNumGhostSelfparent t = if stIsLeaf t 
    then 0
    else sum (map stNumGhostSelfparent $ stChildren t) 
       + if stWeight t==0 && stNode t `Data.List.elem` map stNode (stChildren t)
        then 1
        else 0

{-# INLINABLE stExtraLeaves #-}
stExtraLeaves :: (Eq dp, SpaceTree t dp) => t dp -> Int
stExtraLeaves t = if stIsLeaf t
    then 0
    else sum (map stExtraLeaves $ stChildren t) 
        + if or $ map (\c -> stNode c==stNode t && stIsLeaf c) $ stChildren t
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
    (stChildren t)

{-# INLINABLE prunefold #-}
prunefold :: SpaceTree t a => (b -> t a -> Bool) -> (a -> b -> b) -> b -> t a -> b
prunefold prune f b t = if prune b t
    then b
    else if stIsLeaf t
        then b'
        else foldl' (prunefold prune f) b' (stChildren t) 
    where
        b' = f (stNode t) b

{-# INLINABLE prunefoldW #-}
prunefoldW :: SpaceTree t a => (b -> t a -> Bool) -> (Weighted a -> b -> b) -> b -> t a -> b
prunefoldW prune f b t = if prune b t
    then b
    else if stIsLeaf t
        then b'
        else foldl' (prunefoldW prune f) b' (stChildren t) 
    where
        b' = f (stNodeW t) b

{-# INLINABLE prunefoldA #-}
prunefoldA :: SpaceTree t a => (t a -> b -> Strict.Maybe b) -> b -> t a -> b
prunefoldA !f !b !t = {-# SCC prunefoldA #-} case f t b of
    Strict.Nothing -> b
    Strict.Just b' -> VG.foldl' (prunefoldA f) b' (stChildren_ t)

{-# INLINABLE prunefoldB #-}
prunefoldB :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Strict.Maybe b) -> b -> t a -> b
prunefoldB !f1 !f2 !b !t = {-# SCC prunefoldB #-} case f2 t b of
    Strict.Nothing -> {-# SCC prunefoldB_Nothing #-} b
    Strict.Just b' -> {-# SCC prunefoldB_Just #-} VG.foldl' (prunefoldB f1 f2) b'' (stChildren_ t)
        where
            b'' = {-# SCC b'' #-} VG.foldr' f1 b' (stNodeV t)

{-# INLINABLE prunefoldC #-}
prunefoldC :: SpaceTree t a => (a -> b -> b) -> (t a -> b -> Strict.Either b b) -> b -> t a -> b
prunefoldC !f1 !f2 !b !t = case f2 t b of
    Strict.Left b' -> b'
    Strict.Right b' -> VG.foldl' (prunefoldC f1 f2) b'' (stChildren_ t)
        where
            b'' = VG.foldr' f1 b' (stNodeV t)

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
    (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))

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
            (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))
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
            (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))
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

instance HasRing (sg tag dp) => HasRing (AddUnit sg tag dp) where
    type Ring (AddUnit sg tag dp) = Ring (sg tag dp)

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

instance NumDP (sg tag dp) => NumDP (AddUnit sg tag dp) where
    numdp Unit = 0
    numdp (UnitLift x) = numdp x

instance (SpaceTree (sg tag) dp, NumDP (sg tag dp)) => SpaceTree (AddUnit sg tag) dp where
    type LeafVector (AddUnit sg tag) = LeafVector (sg tag)
    {-# INLINE stMinDistance #-}
    {-# INLINE stMaxDistance #-}
    {-# INLINE stMinDistanceWithDistance #-}
    {-# INLINE stMaxDistanceWithDistance #-}
    {-# INLINE stMinDistanceDp #-}
    {-# INLINE stMaxDistanceDp #-}
    {-# INLINE stMinDistanceDpWithDistance #-}
    {-# INLINE stMaxDistanceDpWithDistance #-}
    {-# INLINE stChildren #-}
    {-# INLINE stChildren' #-}
    {-# INLINE stChildren_ #-}
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

    stMinDistanceDpWithDistance Unit dp = 0 Strict.:!: 0
    stMinDistanceDpWithDistance (UnitLift x) dp = stMinDistanceDpWithDistance x dp
    stMaxDistanceDpWithDistance Unit dp = infinity Strict.:!: infinity
    stMaxDistanceDpWithDistance (UnitLift x) dp = stMaxDistanceDpWithDistance x dp

    stIsMinDistanceDpFartherThanWithDistance (UnitLift x) dp b = stIsMinDistanceDpFartherThanWithDistance x dp b
    stIsMaxDistanceDpFartherThanWithDistance (UnitLift x) dp b = stIsMaxDistanceDpFartherThanWithDistance x dp b

    stMinDistanceDpFromDistance Unit _ _ = 0
    stMinDistanceDpFromDistance (UnitLift x) dp dist = stMinDistanceDpFromDistance x dp dist
    stMaxDistanceDpFromDistance Unit _ _ = infinity
    stMaxDistanceDpFromDistance (UnitLift x) dp dist = stMaxDistanceDpFromDistance x dp dist

    stChildren Unit = []
    stChildren (UnitLift x) = map UnitLift $ stChildren x

    stChildren' Unit = Strict.Nil
    stChildren' (UnitLift x) = fmap UnitLift $ stChildren' x

    stChildren_ Unit = mempty
    stChildren_ (UnitLift x) = fmap UnitLift $ stChildren_ x

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
