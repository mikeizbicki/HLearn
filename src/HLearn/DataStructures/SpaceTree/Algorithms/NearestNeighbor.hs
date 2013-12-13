{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    Neighbor (..)
    
    , NeighborList (..)
    , mkNeighborList

    , NeighborMap (..)
    , nm2list

--     , KNN (..)

    -- * functions
    , findNeighborMap
    , parFindNeighborMap
    , findNeighborList
    , findNeighborListWith 

    , getknnL

--     , knn_vector
--     , knn2_single
--     , knn2_single_parallel
--     , knn_batch 
-- 
--     -- * tmp
    , nl_maxdist
    )
    where

import Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.DeepSeq
import Data.Int
import Data.List
import Data.Maybe 
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Function.Memoize

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..),strictlist2list)

-------------------------------------------------------------------------------
-- data types 

data Neighbor dp = Neighbor
    { neighbor         :: !dp
--     , weight           :: !(Ring dp)
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

newtype NeighborList (k::Nat) dp = NeighborList { getknn :: Strict.List (Neighbor dp) }

mkNeighborList :: Num (Ring dp) => dp -> Ring dp -> NeighborList k dp
-- mkNeighborList dp dist = NeighborList $ Neighbor dp 1 dist :. Strict.Nil
mkNeighborList dp dist = NeighborList $ Neighbor dp dist :. Strict.Nil

getknnL :: NeighborList k dp -> [Neighbor dp]
getknnL = strictlist2list . getknn

deriving instance (Read dp, Read (Ring dp)) => Read (NeighborList k dp)
deriving instance (Show dp, Show (Ring dp)) => Show (NeighborList k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (NeighborList k dp)

nl_maxdist :: forall k dp. (SingI k, Fractional (Ring dp)) => NeighborList k dp -> Ring dp
nl_maxdist (NeighborList Strict.Nil) = infinity
nl_maxdist (NeighborList (x:.Strict.Nil)) = neighborDistance x
nl_maxdist (NeighborList xs ) = neighborDistance $ Strict.last xs

---------------------------------------

newtype NeighborMap (k::Nat) dp = NeighborMap 
    { nm2map :: Map.Map dp (NeighborList k dp)
    }

deriving instance (Read dp, Read (Ring dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Ring dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Ring dp)) => NFData (NeighborMap k dp)

nm2list :: NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = Map.assocs nm

-------------------------------------------------------------------------------
-- algebra

instance (SingI k, MetricSpace dp, Eq dp) => Monoid (NeighborList k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborList Strict.Nil 
    mappend (NeighborList Strict.Nil     ) (NeighborList Strict.Nil     ) = 
        {-# SCC mappend_NeighborList #-} NeighborList Strict.Nil
    mappend (NeighborList (x:.Strict.Nil)) (NeighborList Strict.Nil     ) = 
        {-# SCC mappend_NeighborList #-} NeighborList $ x:.Strict.Nil
    mappend (NeighborList Strict.Nil     ) (NeighborList (y:.Strict.Nil)) = 
        {-# SCC mappend_NeighborList #-} NeighborList $ y:.Strict.Nil
    mappend (NeighborList (x:.xs)  ) (NeighborList (y:.ys)  ) = {-# SCC mappend_NeighborList #-} case k of
        1 -> if x < y then NeighborList (x:.Strict.Nil) else NeighborList (y:.Strict.Nil)
        otherwise -> NeighborList $ Strict.take k $ interleave (x:.xs) (y:.ys)
        where
            k=fromIntegral $ fromSing (sing :: Sing k)

            interleave !xs Strict.Nil = xs
            interleave Strict.Nil !ys = ys
            interleave (x:.xs) (y:.ys) = case compare x y of
                LT -> x:.(interleave xs (y:.ys))
                GT -> y:.(interleave (x:.xs) ys)
                EQ -> if neighbor x == neighbor y
                    then x:.interleave xs ys
                    else x:.(y:.(interleave xs ys))

instance (SingI k, MetricSpace dp, Ord dp) => Monoid (NeighborMap k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborMap mempty
    mappend (NeighborMap x) (NeighborMap y) = 
        {-# SCC mappend_NeighborMap #-} NeighborMap $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList :: (SingI k, SpaceTree t dp, Eq dp) => dp -> t dp -> NeighborList k dp
findNeighborList query t = findNeighborListWith query mempty t

{-# INLINABLE findNeighborListWith #-}
findNeighborListWith :: 
    ( SingI k
    , SpaceTree t dp
    , Eq dp
    ) => dp -> NeighborList k dp -> t dp -> NeighborList k dp
findNeighborListWith query knn t = prunefoldB (knn_catadp query) (knn_cata query) knn t

{-# INLINABLE knn_catadp #-}
knn_catadp :: forall k dp.
    ( SingI k
    , MetricSpace dp
    , Eq dp
    ) => dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !query !dp !knn = {-# SCC knn_catadp2 #-}
    case isFartherThanWithDistance dp query (nl_maxdist knn) of
        Strict.Nothing -> knn
        Strict.Just dist -> if dp==query 
            then knn
            else knn <> (NeighborList $ (Neighbor dp dist):.Strict.Nil)

{-# INLINABLE knn_cata #-}
knn_cata :: forall k t dp. 
    ( SingI k
    , SpaceTree t dp
    , Eq dp
    ) => dp -> t dp -> NeighborList k dp -> Strict.Maybe (NeighborList k dp)
knn_cata !query !t !knn = {-# SCC knn_cata2 #-} 
    case stIsMinDistanceDpFartherThanWithDistance t query (nl_maxdist knn) of
        Strict.Nothing -> Strict.Nothing
        Strict.Just dist -> if stNode t==query 
            then Strict.Just knn
            else Strict.Just $ knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)

---------------------------------------

{-# INLINABLE findNeighborMap #-}
findNeighborMap :: 
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    ) => DualTree (t dp) -> NeighborMap k dp
-- findNeighborMap dual = F.foldMap (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList dp $ reference dual) (query dual)
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList dp $ reference dual) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap :: 
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    , NFData (Ring dp)
    , NFData dp
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} (parallel reduce) $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList dp $ reference dual) (stToList $ query dual)

