{-# LANGUAGE DataKinds,UnboxedTuples,MagicHash #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    Neighbor (..)
    , ValidNeighbor (..)
    
    , NeighborList (..)
    , mkNeighborList
    , getknnL
    , nlMaxDist

    , NeighborMap (..)
    , nm2list

--     , NeighborList (..)

    -- * functions
--     , findNeighborMap
    , findNeighborVec
    , findNeighborSL

    , parFindNeighborMap
--     , parFindNeighborMapWith
--     , parFindEpsilonNeighborMap
    , parFindEpsilonNeighborMapWith
    , findNeighborList
--     , findNeighborListWith 
--     , findEpsilonNeighborList
    , findEpsilonNeighborListWith 
--     , findNeighborList_batch


--     , knn_vector
--     , knn2_single
--     , knn2_single_parallel
--     , knn_batch 
-- 
--     -- * tmp
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
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable as F
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
-- import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM

import HLearn.UnsafeVector

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..),strictlist2list)
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------

data Neighbor dp = Neighbor
--     { neighbor         :: !dp
--     , neighborDistance :: !(Scalar dp)
    { neighbor         :: {-#UNPACK#-}!(L2 VU.Vector Float)
    , neighborDistance :: {-#UNPACK#-}!Float
    }

class 
    ( dp ~ L2 VU.Vector Float
    ) => ValidNeighbor dp 

instance ValidNeighbor (L2 VU.Vector Float)

deriving instance (Read dp, Read (Scalar dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance Eq (Scalar dp) => Eq (Neighbor dp) where
    a == b = neighborDistance a == neighborDistance b

instance Ord (Scalar dp) => Ord (Neighbor dp) where
    compare a b = compare (neighborDistance a) (neighborDistance b)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

------------------------------------------------------------------------------

data NeighborList (k::Nat) dp 
    = NL_Nil
    | NL_Cons {-#UNPACK#-}!(Neighbor dp) !(NeighborList k dp)

deriving instance (Read dp, Read (Scalar dp)) => Read (NeighborList k dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (NeighborList k dp)

instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborList k dp) where
    rnf NL_Nil = ()
    rnf (NL_Cons n ns) = deepseq n $ rnf ns

{-# INLINE nlSingleton #-}
nlSingleton :: 
    ( ValidNeighbor dp
    ) => Neighbor dp -> NeighborList k dp
nlSingleton n = NL_Cons n NL_Nil

{-# INLINE mkNeighborList #-}
mkNeighborList :: 
    ( ValidNeighbor dp
    ) => dp -> Scalar dp -> NeighborList k dp
mkNeighborList !dp !dist = NL_Cons (Neighbor dp dist) NL_Nil

{-# INLINE getknnL #-}
getknnL :: 
    ( ValidNeighbor dp
    ) => NeighborList k dp -> [Neighbor dp]
getknnL NL_Nil = []
getknnL (NL_Cons n ns) = n:getknnL ns

{-# INLINE nlMaxDist #-}
nlMaxDist :: forall k dp. 
    ( KnownNat k
    , ValidNeighbor dp
    , Fractional (Scalar dp)
    ) => NeighborList k dp -> Scalar dp
nlMaxDist nl = go nl
    where
        go NL_Nil = infinity
        go (NL_Cons n NL_Nil) = neighborDistance n
        go (NL_Cons n ns) = go ns

instance CanError (NeighborList k dp) where
    {-# INLINE errorVal #-}
    errorVal = NL_Nil

    {-# INLINE isError #-}
    isError NL_Nil = True
    isError _ = False

instance (KnownNat k, MetricSpace dp, Eq dp) => Monoid (NeighborList k dp) where
    {-# INLINE mempty #-}
    mempty = NL_Nil

    {-# INLINE mappend #-}
    mappend nl1 NL_Nil = nl1
    mappend NL_Nil nl2 = nl2
    mappend (NL_Cons n1 ns1) (NL_Cons n2 ns2) = if neighborDistance n1 > neighborDistance n2
        then NL_Cons n2 ns2 
        else NL_Cons n1 ns1

--     mappend (NeighborList (x:.xs)  ) (NeighborList (y:.ys)  ) = {-# SCC mappend_NeighborList #-} case k of
--         1 -> if x < y then NeighborList (x:.Strict.Nil) else NeighborList (y:.Strict.Nil)
--         otherwise -> NeighborList $ Strict.take k $ interleave (x:.xs) (y:.ys)
--         where
--             k=fromIntegral $ natVal (Proxy :: Proxy k)
-- 
--             interleave !xs Strict.Nil = xs
--             interleave Strict.Nil !ys = ys
--             interleave (x:.xs) (y:.ys) = case compare x y of
--                 LT -> x:.(interleave xs (y:.ys))
--                 GT -> y:.(interleave (x:.xs) ys)
--                 EQ -> if neighbor x == neighbor y
--                     then x:.interleave xs ys
--                     else x:.(y:.(interleave xs ys))

-------------------------------------------------------------------------------

newtype NeighborMap (k::Nat) dp = NeighborMap 
--     { nm2map :: Map.Map dp (NeighborList k dp)
    { nm2map :: HashMap.HashMap dp (NeighborList k dp)
    }

-- deriving instance (Read dp, Read (Scalar dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Scalar dp), Hashable dp, Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborMap k dp)

{-# INLINE nm2list #-}
nm2list :: NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = HashMap.toList nm
-- nm2list (NeighborMap nm) = Map.assocs nm

instance (KnownNat k, MetricSpace dp, Ord dp, Hashable dp) => Monoid (NeighborMap k dp) where
    {-# INLINE mempty #-}
    mempty = NeighborMap mempty

    {-# INLINE mappend #-}
    mappend (NeighborMap x) (NeighborMap y) = 
        {-# SCC mappend_NeighborMap #-} NeighborMap $ HashMap.unionWith (<>) x y

-------------------------------------------------------------------------------
-- single tree

{-# INLINE findNeighborList  #-}
findNeighborList :: 
    ( KnownNat k
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => t dp -> dp -> NeighborList k dp
findNeighborList !t !query = findNeighborListWith mempty t query

{-# INLINE findNeighborListWith #-}
findNeighborListWith :: 
    ( KnownNat k
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> t dp -> dp -> NeighborList k dp
findNeighborListWith !nl !t !q = findEpsilonNeighborListWith nl 0 t q 

-- {-# INLINABLE findEpsilonNeighborList #-}
-- findEpsilonNeighborList !e !t !q = findEpsilonNeighborListWith mempty e t q

{-# INLINE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith :: 
    ( KnownNat k
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query = 
    {-# SCC findEpsilonNeighborListWith #-} prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
    where
        !smudge = 1/(1+epsilon)

-- {-# INLINABLE findNeighborList_batch #-}
-- findNeighborList_batch v st = fmap (findNeighborList st) v

-- {-# INLINABLE knn_catadp #-}
{-# INLINE knn_catadp #-}
knn_catadp :: forall k dp.
    ( KnownNat k
    , MetricSpace dp
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp #-}
    if isError dist 
        then knn
        else if dp==query 
            then knn
            else knn <> nlSingleton (Neighbor dp dist)
    where
        !dist = isFartherThanWithDistanceCanError dp query $! nlMaxDist knn * smudge

-- {-# INLINABLE knn_cata #-}
{-# INLINE knn_cata #-}
knn_cata :: forall k t dp. 
    ( KnownNat k
    , SpaceTree t dp
    , Floating (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-} 
    if stNode t==query 
        then if isError knn
            then nlSingleton (Neighbor (stNode t) infinity)
            else knn
        else if isError dist 
            then errorVal
            else knn <> nlSingleton (Neighbor (stNode t) dist)
    where
        !dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query $! nlMaxDist knn * smudge

---------------------------------------

{-# INLINABLE findNeighborVec #-}
findNeighborVec :: 
    ( SpaceTree t dp 
    , KnownNat k
    , ValidNeighbor dp
    ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
findNeighborVec dual = VG.fromList $ map (findNeighborList $ reference dual) $ stToList $ query dual 

{-# INLINABLE findNeighborSL #-}
findNeighborSL :: 
    ( SpaceTree t dp 
    , KnownNat k
    , ValidNeighbor dp
    ) => DualTree (t dp) -> Strict.List (NeighborList k dp)
findNeighborSL dual = Strict.list2strictlist $ map (findNeighborList $ reference dual) $ stToList $ query dual 

{-# INLINABLE findNeighborVecM #-}
findNeighborVecM :: 
    ( SpaceTree t dp 
    , KnownNat k
    , ValidNeighbor dp
    ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
findNeighborVecM dual = runST $ do
    let qvec = V.fromList $ stToList $ query $ dual
    ret <- VM.new (VG.length qvec)
    
    let go i = if i >= VG.length qvec
            then return ()
            else do
                let nl = findNeighborList (reference dual) $ qvec VG.! i
                seq nl $ return ()
                VM.write ret i nl
                go $ i+1

    go 0
    VG.unsafeFreeze ret 


{-# INLINABLE findNeighborVec' #-}
findNeighborVec' :: 
    ( SpaceTree t dp 
    , KnownNat k
    , ValidNeighbor dp
    ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
findNeighborVec' dual = V.generate (VG.length qvec) go
    where 
        go i = findNeighborList (reference dual) $ qvec VG.! i

        qvec = V.fromList $ stToList $ query $ dual

---------------------------------------

{-# INLINABLE findNeighborMap #-}
findNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => DualTree (t dp) -> NeighborMap k dp
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $ 
    map (\dp -> NeighborMap $ HashMap.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} (parallel reduce) $ 
    map (\dp -> NeighborMap $ HashMap.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

-- {-# INLINABLE parFindNeighborMapWith #-}
-- parFindNeighborMapWith ::
--     ( KnownNat k
--     , SpaceTree t dp
--     , Ord dp
--     , NFData (Scalar dp)
--     , NFData dp
--     , Floating (Scalar dp)
--     , CanError (Scalar dp)
--     ) => NeighborMap k dp -> DualTree (t dp) -> NeighborMap k dp
-- parFindNeighborMapWith (NeighborMap nm) dual = (parallel reduce) $
--     map 
--         (\dp -> NeighborMap $ HashMap.singleton dp $ findNeighborListWith (HashMap.findWithDefault mempty dp nm) (reference dual) dp) 
--         (stToList $ query dual)
-- 
-- {-# INLINABLE parFindEpsilonNeighborMap #-}
-- parFindEpsilonNeighborMap e d = parFindEpsilonNeighborMapWith mempty e d

{-# INLINABLE parFindEpsilonNeighborMapWith #-}
parFindEpsilonNeighborMapWith ::
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborMap k dp -> Scalar dp -> DualTree (t dp) -> NeighborMap k dp
parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ HashMap.singleton dp $ findEpsilonNeighborListWith (HashMap.lookupDefault mempty dp nm) epsilon (reference dual) dp) 
        (stToList $ query dual)

