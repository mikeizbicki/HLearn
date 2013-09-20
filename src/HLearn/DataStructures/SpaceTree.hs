{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree
    where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import HLearn.Algebra

-------------------------------------------------------------------------------
-- SpaceTree 

class (MetricSpace dp) => SpaceTree t dp where
    stMinDistance :: t dp -> t dp -> Ring dp
    stMaxDistance :: t dp -> t dp -> Ring dp
    
    stMinDistanceDp :: t dp -> dp -> Ring dp
    stMaxDistanceDp :: t dp -> dp -> Ring dp

    stChildren :: t dp -> [t dp]
    stNode :: t dp -> dp
    stIsLeaf :: t dp -> Bool

--     stParentMap :: t dp -> Map.Map (t dp) (t dp)

---------------------------------------
-- single tree algs

prunefold1 :: SpaceTree t dp => (dp -> t dp -> Bool) -> (dp -> dp -> dp) -> t dp -> dp
prunefold1 prune f t = foldl' (prunefold prune f) (stNode t) (stChildren t)

prunefoldinit :: SpaceTree t dp => (t dp -> res) -> (res -> t dp -> Bool) -> (dp -> res -> res) -> t dp -> res
prunefoldinit init prune f t = foldl'
    (prunefold prune f)
    (init t)
    (stChildren t)

prunefold :: SpaceTree t a => (b -> t a -> Bool) -> (a -> b -> b) -> b -> t a -> b
prunefold prune f b t = if prune b t
    then b
    else foldl' (prunefold prune f) b' (stChildren t) 
    where
        b' = f (stNode t) b

noprune :: a -> b -> Bool
noprune _ _ = False

---------------------------------------
-- dual tree algs

data DualTree a = DualTree 
    { reference :: a
    , query :: a
    }
    deriving (Read,Show,Eq,Ord)

dualNodes :: SpaceTree t dp => DualTree (t dp) -> DualTree dp
dualNodes dual = DualTree (stNode $ reference dual) (stNode $ query dual)

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

prunefold2 :: 
    ( SpaceTree t dp 
    ) =>(res -> DualTree (t dp) -> Bool) 
    -> (DualTree dp -> res -> res) 
    -> res 
    -> DualTree (t dp) 
    -> res
prunefold2 prune f b pair = if prune b pair || (stIsLeaf (reference pair) && stIsLeaf (query pair))
    then b'
    else foldl' (prunefold2 prune f) b' (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))
    where
        b' = f (dualNodes pair) b

dualTreeMatrix :: [a] -> [a] -> [DualTree a]
dualTreeMatrix xs [] = []
dualTreeMatrix xs (y:ys) = fmap (\x -> DualTree x y) xs ++ dualTreeMatrix xs ys

