-- | The space tree abstraction was pioneered by the <http://mlpack.org MLPack> library.
-- It provides a generic interface for all tree structures that support nearest neighbor queries.
-- The paper <http://arxiv.org/abs/1304.4327 Tree Independent Dual Tree Algorithms> gives full details.
--
-- FIXME:
-- Should this interface be incorporated into subhask's "Container" class hierarchy?
--
-- FIXME:
-- There is a close relation between the pruning folds described in the MLPack paper and the theory of ana/catamorphisms that haskellers love.
-- Making this explicit would require some serious work, but would (maybe) provide an even simpler interface.
module HLearn.Data.SpaceTree
    (
    -- * Type classes
    SpaceTree (..)

    -- * Generic algorithms
    , stToList
--     , stToListW
    , stHasNoChildren
    , stChildrenList
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
    )
    where

import qualified Data.List as L

import Prelude (map)
import SubHask
import SubHask.Algebra.Array
import SubHask.Monad
import SubHask.Compatibility.Containers

import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- SpaceTree

class
    ( Metric dp
    , Bounded (Scalar dp)
    , Logic (t dp) ~ Bool
    , Logic dp ~ Bool
    , Logic (LeafContainer t dp) ~ Bool
    , Logic (LeafContainer t (t dp)) ~ Bool
    , Logic (ChildContainer t (t dp)) ~ Bool
    , Eq_ (t dp)
    , Eq_ (ChildContainer t (t dp))
--     , Scalar dp ~ Scalar (t dp)
    , Elem (ChildContainer t (t dp)) ~ t dp
    , Elem (LeafContainer t dp) ~ dp
    , Constructible (LeafContainer t dp)
    , Constructible  (ChildContainer t (t dp))
    , Normed (LeafContainer t dp)
    , Normed (ChildContainer t (t dp))
    , Foldable (LeafContainer t dp)
    , Foldable (ChildContainer t (t dp))
    , dp ~ Elem (t dp)
    ) => SpaceTree t dp
        where

    type LeafContainer t :: * -> *
    type LeafContainer t = BArray

    type ChildContainer t :: * -> *
    type ChildContainer t = BArray

    stChildren  :: t dp -> ChildContainer t (t dp)
    stLeaves    :: t dp -> LeafContainer t dp

    stNode      :: t dp -> dp
    stWeight    :: t dp -> Scalar dp

    {-# INLINE stNodeW #-}
    stNodeW :: t dp -> Labeled' dp (Scalar dp)
    stNodeW t = Labeled' (stNode t) (stWeight t)

    {-# INLINABLE stMaxDescendentDistance #-}
    stMaxDescendentDistance :: t dp -> Scalar dp
    stMaxDescendentDistance t = maxBound

-------------------------------------------------------------------------------

instance
    ( Metric dp
    , Logic dp~Bool
    , Bounded (Scalar dp)
    ) => SpaceTree BArray dp
        where

    type LeafContainer BArray = BArray
    type ChildContainer BArray = BArray

    stChildren _ = empty
    stLeaves = id
    stNode = (!0)
    stWeight = 1

-------------------------------------------------------------------------------
-- generic algorithms

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

{-# INLINABLE stHasNoChildren #-}
stHasNoChildren :: SpaceTree t dp => t dp -> Bool
stHasNoChildren t = isEmpty $ stChildren t

{-# INLINABLE stChildrenList #-}
stChildrenList :: SpaceTree t dp => t dp -> [t dp]
stChildrenList t = toList $ stChildren t

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
