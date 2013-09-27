{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree
    where

import Control.DeepSeq
import Data.Semigroup
import Data.List
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set

import HLearn.Algebra hiding ((<>))
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- SpaceTree 

class HasTag t where
    type Tag t
    getTag :: t -> Maybe (Tag t)
    setTag :: Tag t -> t -> t

class (MetricSpace dp) => SpaceTree t dp where
    stMinDistance :: t dp -> t dp -> Ring dp
    stMaxDistance :: t dp -> t dp -> Ring dp
    
    stMinDistanceDp :: t dp -> dp -> Ring dp
    stMaxDistanceDp :: t dp -> dp -> Ring dp

    stChildren :: t dp -> [t dp]
    stNode :: t dp -> dp
    stHasNode :: t dp -> Bool
    stIsLeaf :: t dp -> Bool

stDescendents :: SpaceTree t dp => t dp -> [dp]
stDescendents t = if stIsLeaf t 
    then [stNode t]
    else concatMap stDescendents $ stChildren t

stNumNodes :: SpaceTree t dp => t dp -> Int
-- stNumNodes = prunefold noprune (\ _ y -> y+1) 0
stNumNodes t = if stIsLeaf t
    then 1
    else 1 + sum (map stNumNodes $ stChildren t)

stMaxChildren :: SpaceTree t dp => t dp -> Int
stMaxChildren t = if stIsLeaf t
    then 0
    else maximum $ (length $ stChildren t):(map stMaxChildren $ stChildren t)

stAveChildren :: SpaceTree t dp => t dp -> Normal Double Double
stAveChildren t = if stIsLeaf t
    then mempty
    else (train1dp . fromIntegral . length $ stChildren t) `mappend` (reduce . map stAveChildren $ stChildren t)

stMaxDepth :: SpaceTree t dp => t dp -> Int
stMaxDepth t = if stIsLeaf t
    then 1
    else 1+maximum (map stMaxDepth $ stChildren t)

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
    then b'
    else if stIsLeaf t
        then b'
        else foldl' (prunefold prune f) b' (stChildren t) 
    where
        b' = f (stNode t) b

noprune :: b -> a -> Bool
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

data AddUnit sg dp
    = Unit 
    | UnitLift (sg dp)
    deriving (Read,Show,Eq,Ord)

instance NFData (sg dp) => NFData (AddUnit sg dp) where
    rnf Unit = ()
    rnf (UnitLift sg) = rnf sg

instance Semigroup (sg dp) => Monoid (AddUnit sg dp) where
    mempty = Unit
    mappend Unit x = x
    mappend x Unit = x
    mappend (UnitLift x) (UnitLift y) = UnitLift $ x<>y
 
instance F.Foldable sg => F.Foldable (AddUnit sg) where
    foldr f i Unit = error "foldr Unit"
    foldr f i (UnitLift x) = F.foldr f i x

instance SpaceTree sg dp => SpaceTree (AddUnit sg) dp where
    stMinDistance Unit x = 0
    stMinDistance x Unit = 0
    stMinDistance (UnitLift x) (UnitLift y) = stMinDistance x y

    stMaxDistance Unit x = infinity
    stMaxDistance x Unit = infinity
    stMaxDistance (UnitLift x) (UnitLift y) = stMaxDistance x y

    stMinDistanceDp Unit x = 0
    stMinDistanceDp (UnitLift x) dp = stMinDistanceDp x dp

    stMaxDistanceDp Unit x = infinity
    stMaxDistanceDp (UnitLift x) dp = stMaxDistanceDp x dp

    stChildren Unit = []
    stChildren (UnitLift x) = map UnitLift $ stChildren x

    stNode Unit = error "stNode Unit"
    stNode (UnitLift x) = stNode x

    stHasNode Unit = False
    stHasNode (UnitLift x) = stHasNode x

    stIsLeaf Unit = False
    stIsLeaf (UnitLift x) = stIsLeaf x 
