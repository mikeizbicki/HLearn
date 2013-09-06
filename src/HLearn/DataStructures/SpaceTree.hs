{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree
    where

import Data.List

import GHC.TypeLits

import HLearn.Algebra

-------------------------------------------------------------------------------
-- 

class (MetricSpace dp) => SpaceTree t dp where
    stMinDistance :: t dp -> t dp -> Ring dp
    stMaxDistance :: t dp -> t dp -> Ring dp
    stChildren :: t dp -> [t dp]
    stNode :: t dp -> dp

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

prunefold2init :: SpaceTree t dp =>
    (DualTree (t dp) -> res) -> 
        (res -> DualTree (t dp) -> Bool) -> (DualTree dp -> res -> res) -> DualTree (t dp) -> res
prunefold2init init prune f pair = foldl' 
    (prunefold2 prune f) 
    (init pair) 
    (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))

prunefold2 :: SpaceTree t dp => 
    (res -> DualTree (t dp) -> Bool) -> (DualTree dp -> res -> res) -> res -> DualTree (t dp) -> res
prunefold2 prune f b pair = if prune b pair
    then b
    else foldl' (prunefold2 prune f) b' (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))
    where
        b' = f (DualTree (stNode $ reference pair) (stNode $ query pair)) b

dualTreeMatrix :: [a] -> [a] -> [DualTree a]
dualTreeMatrix xs [] = []
dualTreeMatrix xs (y:ys) = fmap (\x -> DualTree x y) xs ++ dualTreeMatrix xs ys

-------------------------------------------------------------------------------
-- nearest neighbor 

newtype KNN (k::Nat) dp = KNN { getknn :: [Neighbor dp] }

deriving instance (Read dp, Read (Ring dp)) => Read (KNN k dp)
deriving instance (Show dp, Show (Ring dp)) => Show (KNN k dp)

instance (SingI k, MetricSpace dp, Eq dp) => Monoid (KNN k dp) where
    mempty = KNN []
    mappend (KNN xs) (KNN ys) = KNN $ take k $ interleave xs ys
        where
            k=fromIntegral $ fromSing (sing :: Sing k)

knnFull :: forall k dp. SingI k => KNN k dp -> Bool
knnFull knn = length (getknn knn) > k
    where
        k = fromIntegral $ fromSing (sing :: Sing k)

knnMaxDistance :: KNN k dp -> Ring dp
knnMaxDistance (KNN xs) = neighborDistance $ last xs

init_knn :: SpaceTree t dp => dp -> t dp -> KNN k dp
init_knn query t = KNN [Neighbor (stNode t) (distance (stNode t) query)]

-- interleave :: Ord a => [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = case compare x y of
    LT -> x:(interleave xs (y:ys))
    GT -> y:(interleave (x:xs) ys)
    EQ -> if neighbor x == neighbor y
        then x:interleave xs ys
        else x:y:interleave xs ys

---------------------------------------

data Neighbor dp = Neighbor
    { neighbor         :: !dp
    , neighborDistance :: !(Ring dp)
    }

deriving instance (Read dp, Read (Ring dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Ring dp)) => Show (Neighbor dp)

instance Eq (Ring dp) => Eq (Neighbor dp) where
    a == b = neighborDistance a == neighborDistance b

instance Ord (Ring dp) => Ord (Neighbor dp) where
    compare a b = compare (neighborDistance a) (neighborDistance b)

init_neighbor :: SpaceTree t dp => dp -> t dp -> Neighbor dp
init_neighbor query t = Neighbor
    { neighbor = stNode t
    , neighborDistance = distance query (stNode t)
    }

nearestNeighbor :: SpaceTree t dp => dp -> t dp -> Neighbor dp
nearestNeighbor query t = prunefoldinit (init_neighbor query) (nn_prune query) (nn_cata query) t

nearestNeighbor_slow :: SpaceTree t dp => dp -> t dp -> Neighbor dp
nearestNeighbor_slow query t = prunefoldinit undefined noprune (nn_cata query) t

nn_prune :: SpaceTree t dp => dp -> Neighbor dp -> t dp -> Bool
nn_prune query b t = neighborDistance b < distance query (stNode t)

nn_cata :: MetricSpace dp => dp -> dp -> Neighbor dp -> Neighbor dp
nn_cata query next current = if neighborDistance current < nextDistance
    then current
    else Neighbor next nextDistance
    where
        nextDistance = distance query next

knn :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp
knn query t = prunefoldinit (init_knn query) (knn_prune query) (knn_cata query) t

knn_prune :: forall k t dp. (SingI k, SpaceTree t dp) => dp -> KNN k dp -> t dp -> Bool
knn_prune query res t = knnMaxDistance res < distance query (stNode t) && knnFull res

knn_cata :: (SingI k, MetricSpace dp, Eq dp) => dp -> dp -> KNN k dp -> KNN k dp
knn_cata query next current = KNN [Neighbor next $ distance query next] <> current
