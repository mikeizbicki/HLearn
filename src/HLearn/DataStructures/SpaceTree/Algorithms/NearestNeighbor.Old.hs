
{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    where

import Debug.Trace

import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- data types 

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

instance (NFData dp, NFData (Ring dp)) => NFData (Neighbor dp) where
    rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

---------------------------------------

-- newtype KNN (k::Nat) dp = KNN { getknn :: [Neighbor dp] }
newtype KNN (k::Nat) dp = KNN { getknn :: V.Vector (Neighbor dp) }

deriving instance (Read dp, Read (Ring dp)) => Read (KNN k dp)
deriving instance (Show dp, Show (Ring dp)) => Show (KNN k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (KNN k dp)

knn_maxdist :: forall k dp. (SingI k,Ord dp,Fractional (Ring dp)) => KNN k dp -> Ring dp
knn_maxdist (KNN v) = if V.length v > 0
    then neighborDistance $ v V.! (V.length v-1) 
    else inf

inf :: Fractional n => n
inf = 1/0

---------------------------------------

newtype KNN2 (k::Nat) dp = KNN2 
    { getknn2 :: Map.Map dp (KNN k dp) 
    }

deriving instance (Read dp, Read (Ring dp), Ord dp, Read (KNN k dp)) => Read (KNN2 k dp)
deriving instance (Show dp, Show (Ring dp), Ord dp, Show (KNN k dp)) => Show (KNN2 k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (KNN2 k dp)

instance (SpaceTree t dp, Ord dp, SingI k) => Function (KNN2 k dp) (DualTree (t dp)) (KNN2 k dp) where
    function _ = knn2

-------------------------------------------------------------------------------
-- algebra

instance (SingI k, MetricSpace dp, Eq dp) => Monoid (KNN k dp) where
    mempty = KNN mempty
    mappend (KNN v1) (KNN v2) = KNN $ runST $ do
        v' <- VM.new k'
        go v' 0 0 0    
        V.unsafeFreeze v'
        where
            go :: VM.MVector s (Neighbor dp) -> Int -> Int -> Int -> ST s ()
            go v' i i1 i2 = if i>=k'
                then return ()
                else if v1 V.! i1 < v2 V.! i2
                    then VM.write v' i (v1 V.! i1) >> go v' (i+1) (i1+1) i2
                    else VM.write v' i (v2 V.! i2) >> go v' (i+1) i1 (i2+1)
                    
            k'=min k (V.length v1+V.length v2)
            k=fromIntegral $ fromSing (sing :: Sing k)
--
--     mempty = KNN []
--     mappend (KNN xs) (KNN ys) = KNN $ take k $ interleave xs ys
--         where
--             k=fromIntegral $ fromSing (sing :: Sing k)

instance (SingI k, MetricSpace dp, Ord dp) => Monoid (KNN2 k dp) where
    mempty = KNN2 mempty
    mappend (KNN2 x) (KNN2 y) = KNN2 $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- dual tree

knn2 :: (SpaceTree t dp, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2=knn2_fast

knn2_fast :: (SpaceTree t dp, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2_fast = prunefold2init initKNN2 knn2_prune knn2_cata

knn2_slow :: (SpaceTree t dp, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2_slow = prunefold2init initKNN2 noprune knn2_cata

initKNN2 :: SpaceTree t dp => DualTree (t dp) -> KNN2 k dp
initKNN2 dual = KNN2 $ Map.singleton qnode val
    where
        rnode = stNode $ reference dual
        qnode = stNode $ query dual
        val = KNN $ V.singleton $ Neighbor rnode (distance qnode rnode)

knn2_prune :: forall k t dp. (SingI k, SpaceTree t dp, Ord dp) => KNN2 k dp -> DualTree (t dp) -> Bool
knn2_prune knn2 dual = stMinDistance (reference dual) (query dual) > bound
    where
        bound = maxdist knn2 (reference dual)

dist :: forall k t dp. (SingI k, MetricSpace dp, Ord dp) => KNN2 k dp -> dp -> Ring dp
dist knn2 dp = knn_maxdist $ Map.findWithDefault mempty dp $ getknn2 knn2

maxdist :: forall k t dp. (SingI k, SpaceTree t dp, Ord dp) => KNN2 k dp -> t dp -> Ring dp
maxdist knn2 tree = if stIsLeaf tree
    then dist knn2 (stNode tree) 
    else maximum
        $ (dist knn2 (stNode tree))
        : (fmap (maxdist knn2) $ stChildren tree)

knn2_cata :: (SingI k, Ord dp, MetricSpace dp) => DualTree dp -> KNN2 k dp -> KNN2 k dp 
knn2_cata !dual !knn2 = KNN2 $ Map.insertWith (<>) qnode knn' $ getknn2 knn2
    where
        rnode = reference dual 
        qnode = query dual 
        dualdist = distance rnode qnode
        knn' = KNN $ V.singleton $ Neighbor rnode dualdist


-------------------------------------------------------------------------------
-- single tree

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

---------------------------------------

knn :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp
knn query t = prunefoldinit (init_knn query) (knn_prune query) (knn_cata query) t

knn_prune :: forall k t dp. (SingI k, SpaceTree t dp) => dp -> KNN k dp -> t dp -> Bool
knn_prune query res t = knnMaxDistance res < distance query (stNode t) && knnFull res

knn_cata :: (SingI k, MetricSpace dp, Eq dp) => dp -> dp -> KNN k dp -> KNN k dp
knn_cata query next current = KNN (V.singleton (Neighbor next $ distance query next)) <> current

knnFull :: forall k dp. SingI k => KNN k dp -> Bool
knnFull knn = V.length (getknn knn) > k
    where
        k = fromIntegral $ fromSing (sing :: Sing k)

knnMaxDistance :: KNN k dp -> Ring dp
knnMaxDistance (KNN xs) = neighborDistance $ V.last xs

init_knn :: SpaceTree t dp => dp -> t dp -> KNN k dp
init_knn query t = KNN $ V.singleton $ Neighbor (stNode t) (distance (stNode t) query)

interleave :: (Eq a, Ord (Ring a)) => [Neighbor a] -> [Neighbor a] -> [Neighbor a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = case compare x y of
    LT -> x:(interleave xs (y:ys))
    GT -> y:(interleave (x:xs) ys)
    EQ -> if neighbor x == neighbor y
        then x:interleave xs ys
        else x:y:interleave xs ys

---------------------------------------

knn2_single :: (SingI k, SpaceTree t dp, Eq dp, F.Foldable t, Ord dp) => DualTree (t dp) -> KNN2 k dp
knn2_single dual = F.foldMap (\dp -> KNN2 $ Map.singleton dp $ knn dp $ reference dual) (query dual)
