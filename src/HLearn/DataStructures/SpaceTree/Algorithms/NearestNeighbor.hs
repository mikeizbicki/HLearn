{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    Neighbor (..)
    , KNN (..)
    , KNN2 (..)

    -- * functions
    , nearestNeighbor
    , nearestNeighbor_slow

    , knn
    , knn_slow
    , knnA
    , knn_maxdist

    , DualKNNM (..)
    , dknn
    , knn2_cata

    , knn2
    , knn2_single
    , knn2_single_slow
    , knn2_single_parallel
    )
    where

import Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.DeepSeq
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Function.Memoize
-- import Data.MemoCombinators

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

newtype KNN (k::Nat) dp = KNN { getknn :: [Neighbor dp] }

deriving instance (Read dp, Read (Ring dp)) => Read (KNN k dp)
deriving instance (Show dp, Show (Ring dp)) => Show (KNN k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (KNN k dp)

knn_maxdist :: forall k dp. (SingI k,Fractional (Ring dp)) => KNN k dp -> Ring dp
knn_maxdist (KNN []) = infinity
knn_maxdist (KNN xs) = neighborDistance $ last xs

-------------------

data DualKNNM s (k::Nat) dp = DualKNNM
    { nodevec :: !(VUM.MVector s (Ring dp))
    , dpvec :: !(VM.MVector s (KNN k dp))
    }

-- deriving instance (Show dp, Show (Ring dp), Show (vec (KNN k dp)), Show (vec (Ring dp))) => Show (DualKNN k dp vec)
-- instance (NFData dp, NFData (Ring dp), NFData (vec (KNN k dp)), NFData (vec (Ring dp))) => NFData (DualKNN k dp vec) where
--     rnf dk = deepseq (nodevec dk)
--            $ rnf (dpvec dk)

-- instance Monoid Int where
--     mempty = error "Monoid Int"

{-# INLINABLE dknn #-}
dknn :: forall t tag dp s k.
    ( SpaceTree (t ()) dp
    , SpaceTree (t (Int,Int)) dp
    , Taggable t dp
    , Ord dp
    , SingI k
    , Show dp, Show (Ring dp)
    , VUM.Unbox (Ring dp)
    , NFData (Ring dp), NFData dp
--     ) => DualTree ((t ()) dp) -> ST s (DualKNNM s k dp)
--     ) => DualTree ((t ()) dp) -> ST s (KNN2 k dp)
    ) => DualTree ((t ()) dp) -> KNN2 k dp
dknn dual = runST $ do
    let (r,_,_) = initTags $ reference dual
    let (q,numnodes,numdp) = initTags $ query dual
    let dual' = DualTree
            { reference = r
            , query = q
            }
    nodevec' <- VGM.replicate numnodes infinity
    dpvec' <- VGM.replicate numdp mempty
    let knnm = DualKNNM
            { nodevec = nodevec' 
            , dpvec = dpvec' 
            }
    
    dualfoldM dualknn_tag dualknn_prune dualknn_cata knnm dual'
--     dualfoldM (\_ -> return) (\_ _ -> return False) dualknn_cata knnm dual'
    xs <- go knnm q
    return $ KNN2 $ Map.fromList xs

    where
        go knnm q = if stIsLeaf q
            then do
                knn <- VGM.read (dpvec knnm) (dpIndex q)
                return [(stNode q, knn)]
            else fmap concat . mapM (go knnm) $ stChildren q

--     return knnm

dualknn_tag :: 
    ( Taggable t dp
    , SpaceTree (t (Int,Int)) dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Num (Ring dp)
    , SingI k
    , VUM.Unbox (Ring dp)
    ) => DualTree (t (Int,Int) dp) -> DualKNNM s k dp -> ST s (DualKNNM s k dp)
dualknn_tag dual dualknnm = do
    let nodeindex = nodeIndex $ query dual
    let dpindex = dpIndex $ query dual 

    knn <- VGM.unsafeRead (dpvec dualknnm) dpindex 
    bound <- VGM.unsafeRead (nodevec dualknnm) nodeindex

--     childbounds <- forM (stChildren $ query dual) $ \dp -> VGM.unsafeRead (nodevec dualknnm) (nodeIndex dp)
--     let bound = minimum
--             [ if stIsLeaf (query dual)
--                 then knn_maxdist knn
--                 else max (knn_maxdist knn) $ maximum childbounds
--             , knn_maxdist knn + ro (query dual) + lambda (query dual)
--             , minimum childbounds + 2 * (lambda (query dual) - lambda (head $ stChildren $ query dual))
--             ]

    let bound = knn_maxdist knn + ro (query dual) + lambda (query dual)
    VGM.unsafeWrite (nodevec dualknnm) nodeindex bound

    return dualknnm

dualknn_prune ::
    ( Taggable t dp
    , SpaceTree (t (Int,Int)) dp
    , Ord (Ring dp)
    , VUM.Unbox (Ring dp)
    , Show (Ring dp)
    ) => DualKNNM s k dp -> DualTree (t (Int,Int) dp) -> ST s Bool
-- dualknn_prune knnm dual = return False
dualknn_prune knnm dual = do
--     return $ trace "prune?" $ ()
    bound <- VGM.unsafeRead (nodevec knnm) (nodeIndex $ query dual)
    let prune = stMinDistance (reference dual) (query dual) > bound
--     if prune
--         then trace "prune!" $ return ()
--         else trace ("noprune; bound="++show bound) $ return ()
    return prune

dualknn_cata :: 
    ( SingI k
    , Ord dp
    , SpaceTree (t (Int,Int)) dp
    , Taggable t dp
    , Show dp, Show (Ring dp)
    , NFData (Ring dp), NFData dp
    ) => DualTree (t (Int,Int) dp) -> DualKNNM s k dp -> ST s (DualKNNM s k dp)
dualknn_cata dual knnm = do
    knn <- VGM.unsafeRead (dpvec knnm) (dpIndex $ query dual)
    let knnb = KNN [Neighbor rdp (distance rdp qdp)]
    let knn' = if stNode (reference dual) == stNode (query dual)
            then knn
            else knn <> knnb 

--     trace ( "r="++(show $ dpIndex $ reference dual) ++"="++show (stNode $ reference dual)
--         ++"; q="++(show $ dpIndex $ query dual)++"="++show (stNode $ query dual)
--         ++"\n; knn="++show knn
--         ++"\n; knnb="++show knnb
--         ++"\n; knn'="++show knn'
--         ++"\n"
--         ) $ return ()
    deepseq knn' $ VGM.unsafeWrite (dpvec knnm) (dpIndex $ query dual) knn'
    return knnm
    where
        rdp = stNode $ reference dual
        qdp = stNode $ query dual
    
{-# INLINABLE dualfoldM #-}
dualfoldM :: 
    ( SpaceTree t dp 
    , Monad m
    ) 
    => (DualTree (t dp) -> res -> m res)
    -> (res -> DualTree (t dp) -> m Bool) 
    -> (DualTree (t dp) -> res -> m res) 
    -> res 
    -> DualTree (t dp) 
    -> m res
dualfoldM tag prune f b pair = do
    b_tagged <- tag pair b 
    shouldprune <- prune b_tagged pair
--     trace "dualfoldM" $
    if shouldprune
        then return b_tagged
        else do
            b' <- f pair b_tagged
            if stIsLeaf (reference pair) && stIsLeaf (query pair)
                then return b' 
                else foldM
                    (dualfoldM tag prune f) 
                    b' 
                    (dualTreeMatrix (stChildren $ reference pair) (stChildren $ query pair))

dualTreeMatrix :: [a] -> [a] -> [DualTree a]
dualTreeMatrix xs [] = []
dualTreeMatrix xs (y:ys) = fmap (\x -> DualTree x y) xs ++ dualTreeMatrix xs ys

-- knn2_cata dual knn2 = KNN2 $ Map.insertWith (<>) qnode knn' $ getknn2 knn2
--     where
--         rnode = reference dual 
--         qnode = query dual 
--         knn' = if rnode == qnode
--             then mempty
--             else KNN $ [Neighbor rnode $ distance rnode qnode]

---------------------------------------

newtype KNN2 (k::Nat) dp = KNN2 
    { getknn2 :: Map.Map dp (KNN k dp)
    }

deriving instance (Read dp, Read (Ring dp), Ord dp, Read (KNN k dp)) => Read (KNN2 k dp)
deriving instance (Show dp, Show (Ring dp), Ord dp, Show (KNN k dp)) => Show (KNN2 k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (KNN2 k dp)

instance 
    ( SpaceTree t dp
    , F.Foldable t
    , Ord dp
    , SingI k
    ) => Function (KNN2 k dp) (DualTree (t dp)) (KNN2 k dp) 
        where
    function _ = knn2

-------------------------------------------------------------------------------
-- algebra

instance (SingI k, MetricSpace dp, Eq dp) => Monoid (KNN k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = KNN mempty 
    mappend (KNN xs) (KNN ys) = KNN $ take k $ interleave xs ys
        where
            k=fromIntegral $ fromSing (sing :: Sing k)

            interleave :: (Eq a, Ord (Ring a)) => [Neighbor a] -> [Neighbor a] -> [Neighbor a]
            interleave xs [] = xs
            interleave [] ys = ys
            interleave (x:xs) (y:ys) = case compare x y of
                LT -> x:(interleave xs (y:ys))
                GT -> y:(interleave (x:xs) ys)
                EQ -> if neighbor x == neighbor y
                    then x:interleave xs ys
                    else x:y:interleave xs ys

instance (SingI k, MetricSpace dp, Ord dp) => Monoid (KNN2 k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = KNN2 mempty
    mappend (KNN2 x) (KNN2 y) = KNN2 $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- dual tree

---------------------------------------

{-# INLINABLE knn2 #-}
knn2 :: (SpaceTree t dp, F.Foldable t, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2=knn2_single

{-# INLINABLE knn2_fast #-}
knn2_fast :: (SpaceTree t dp, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2_fast = prunefold2 knn2_prune knn2_cata mempty

{-# INLINABLE knn2_slow #-}
knn2_slow :: (SpaceTree t dp, Ord dp, SingI k) => DualTree (t dp) -> KNN2 k dp
knn2_slow = prunefold2 noprune knn2_cata mempty

knn2_prune :: forall k t dp. (SingI k, SpaceTree t dp, Ord dp) => KNN2 k dp -> DualTree (t dp) -> Bool
knn2_prune knn2 dual = stMinDistance (reference dual) (query dual) > bound
    where
        bound = maxdist knn2 (reference dual)

maxdist :: forall k t dp. (SingI k, SpaceTree t dp, Ord dp) => KNN2 k dp -> t dp -> Ring dp
maxdist knn2 tree = if stIsLeaf tree
    then dist knn2 (stNode tree) 
    else maximum
        $ (dist knn2 (stNode tree))
        : (fmap (maxdist knn2) $ stChildren tree)

dist :: forall k t dp. (SingI k, MetricSpace dp, Ord dp) => KNN2 k dp -> dp -> Ring dp
dist knn2 dp = knn_maxdist $ Map.findWithDefault mempty dp $ getknn2 knn2

knn2_cata :: (SingI k, Ord dp, MetricSpace dp) => DualTree dp -> KNN2 k dp -> KNN2 k dp 
knn2_cata !dual !knn2 = KNN2 $ Map.insertWith (<>) qnode knn' $ getknn2 knn2
    where
        rnode = reference dual 
        qnode = query dual 
        knn' = if rnode == qnode
            then mempty
            else KNN $ [Neighbor rnode $ distance rnode qnode]


-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE knn #-}
knn :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp
knn query t = prunefold (knn_prune query) (knn_cata query) mempty t

{-# INLINABLE knn_slow #-}
knn_slow :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp
knn_slow query t = prunefold noprune (knn_cata query) mempty t

knn_prune :: forall k t dp. (SingI k, SpaceTree t dp) => dp -> KNN k dp -> t dp -> Bool
knn_prune query res t = knn_maxdist res < stMinDistanceDp t query && length (getknn res) >= k
    where
        k = fromIntegral $ fromSing (sing :: Sing k)

knn_cata :: (SingI k, MetricSpace dp, Eq dp) => dp -> dp -> KNN k dp -> KNN k dp
knn_cata query next current = if next==query
    then current
    else KNN [Neighbor next $ distance query next] <> current

knnA :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp
knnA query t = prunefoldA (knn_cataA query) mempty t

knn_cataA :: forall k t dp. (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> KNN k dp -> Maybe (KNN k dp)
knn_cataA query t res = if knn_maxdist res < mindist && length (getknn res) >= k
    then Nothing
    else Just $ if stNode t==query
        then res
        else KNN [Neighbor (stNode t) $ dist] <> res 
    where
        k = fromIntegral $ fromSing (sing :: Sing k)
        (mindist,dist) = stMinDistanceDpWithDistance t query

---------------------------------------

{-# INLINABLE knn2_single #-}
knn2_single :: (SingI k, SpaceTree t dp, Eq dp, F.Foldable t, Ord dp) => DualTree (t dp) -> KNN2 k dp
knn2_single dual = F.foldMap (\dp -> KNN2 $ Map.singleton dp $ knn dp $ reference dual) (query dual)

{-# INLINABLE knn2_single_slow #-}
knn2_single_slow :: (SingI k, SpaceTree t dp, Eq dp, F.Foldable t, Ord dp) => DualTree (t dp) -> KNN2 k dp
knn2_single_slow dual = F.foldMap (\dp -> KNN2 $ Map.singleton dp $ knn_slow dp $ reference dual) (query dual)

{-# INLINABLE knn2_single_parallel #-}
knn2_single_parallel :: 
    ( SingI k
    , SpaceTree t dp
    , Eq dp, F.Foldable t, Ord dp
    , NFData (Ring dp), NFData dp
    ) => DualTree (t dp) -> KNN2 k dp
knn2_single_parallel dual = (parallel reduce) $ map (\dp -> KNN2 $ Map.singleton dp $ knnA dp $ reference dual) (F.toList $ query dual)

---------------------------------------

{-# INLINABLE nearestNeighbor #-}
nearestNeighbor :: SpaceTree t dp => dp -> t dp -> Neighbor dp
nearestNeighbor query t = prunefoldinit (nn_init query) (nn_prune query) (nn_cata query) t

{-# INLINABLE nearestNeighbor_slow #-}
nearestNeighbor_slow :: SpaceTree t dp => dp -> t dp -> Neighbor dp
nearestNeighbor_slow query t = prunefoldinit (nn_init query) noprune (nn_cata query) t

nn_init :: SpaceTree t dp => dp -> t dp -> Neighbor dp
nn_init query t = Neighbor
    { neighbor = stNode t
    , neighborDistance = distance query (stNode t)
    }

nn_prune :: SpaceTree t dp => dp -> Neighbor dp -> t dp -> Bool
nn_prune query b t = neighborDistance b < distance query (stNode t)

nn_cata :: MetricSpace dp => dp -> dp -> Neighbor dp -> Neighbor dp
nn_cata query next current = if neighborDistance current < nextDistance
    then current
    else Neighbor next nextDistance
    where
        nextDistance = distance query next
