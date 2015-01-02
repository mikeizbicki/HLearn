{-# LANGUAGE DataKinds,UnboxedTuples,MagicHash,TemplateHaskell,RankNTypes #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    (

    -- * data types
    Neighbor (..)
    , ValidNeighbor (..)

    , NeighborList (..)
    , nlSingleton
    , getknnL
    , nlMaxDist

    , NeighborMap (..)
    , nm2list

    , Param_k
    , _k

    -- * functions
    , findNeighborVec

    , findAllNeighbors

    , parFindNeighborMap
    , parFindEpsilonNeighborMap
    , parFindEpsilonNeighborMapWith
    , findNeighborList
    , findEpsilonNeighborListWith
    )
    where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import SubHask
import SubHask.Algebra.Container
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector.Lebesgue
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

import HLearn.Algebra.Structures.Comonoid
import HLearn.DataStructures.SpaceTree
import HLearn.UnsafeVector

import Data.Params

-------------------------------------------------------------------------------

data Neighbor dp = Neighbor
    { neighbor         :: !dp
    , neighborDistance :: !(Scalar dp)
--     { neighbor         :: {-#UNPACK#-}!(L2 VU.Vector Float)
--     , neighborDistance :: {-#UNPACK#-}!Float
    }

type instance Logic (Neighbor dp) = Bool

-- class
--     ( dp ~ L2 VU.Vector Float
--     ) => ValidNeighbor dp
--
-- instance ValidNeighbor (L2 VU.Vector Float)

type ValidNeighbor dp =
    ( MetricSpace dp
    , Bounded (Scalar dp)
    , CanError (Scalar dp)
    , Logic dp~Bool
--     , dp ~ (L2 VU.Vector Float)
    )

deriving instance (Read dp, Read (Scalar dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance Eq (Scalar dp) => Eq_ (Neighbor dp) where
    {-# INLINE (==) #-}
    a == b = neighborDistance a == neighborDistance b

-- instance Ord (Scalar dp) => Ord (Neighbor dp) where
--     compare a b = compare (neighborDistance a) (neighborDistance b)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

------------------------------------------------------------------------------

data NeighborList (k :: Config Nat) dp
    = NL_Nil
    | NL_Cons {-#UNPACK#-}!(Neighbor dp) !(NeighborList k dp)

mkParams ''NeighborList

-- | update the distances in the NeighborList based on a new data point
resetNL :: ValidNeighbor dp => dp -> NeighborList k dp -> NeighborList k dp
resetNL p NL_Nil = NL_Nil
resetNL p (NL_Cons (Neighbor q _) nl)
    = NL_Cons (Neighbor q $ distance p q) $ resetNL p nl

type instance Logic (NeighborList k dp) = Bool

deriving instance (Read dp, Read (Scalar dp)) => Read (NeighborList k dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (NeighborList k dp)

instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborList k dp) where
    rnf NL_Nil = ()
    rnf (NL_Cons n ns) = deepseq n $ rnf ns

instance (ValidNeighbor dp, Eq_ dp) => Eq_ (NeighborList k dp) where
    (NL_Cons x xs) ==(NL_Cons y ys) = x==y && xs==ys
    NL_Nil == NL_Nil = True
    _ == _ = False

property_orderedNeighborList :: (Logic dp~Bool, MetricSpace dp) => NeighborList k dp -> Bool
property_orderedNeighborList NL_Nil = True
property_orderedNeighborList (NL_Cons n NL_Nil) = True
property_orderedNeighborList (NL_Cons n (NL_Cons n2 ns)) = if neighborDistance n < neighborDistance n2
    then property_orderedNeighborList (NL_Cons n2 ns)
    else False

{-# INLINE nlSingleton #-}
nlSingleton ::
    ( ValidNeighbor dp
    ) => Neighbor dp -> NeighborList k dp
nlSingleton !n = NL_Cons n NL_Nil

-- {-# INLINE mkNeighborList #-}
-- mkNeighborList ::
--     ( ValidNeighbor dp
--     ) => dp -> Scalar dp -> NeighborList k dp
-- mkNeighborList !dp !dist = NL_Cons (Neighbor dp dist) NL_Nil

{-# INLINE getknnL #-}
getknnL ::
    ( ValidNeighbor dp
    ) => NeighborList k dp -> [Neighbor dp]
getknnL NL_Nil = []
getknnL (NL_Cons n ns) = n:getknnL ns

{-# INLINE nlMaxDist #-}
nlMaxDist ::
    ( ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp
nlMaxDist !nl = go nl
    where
        go NL_Nil = maxBound
        go (NL_Cons n NL_Nil) = neighborDistance n
        go (NL_Cons n ns) = go ns

{-# INLINE nlAddNeighbor #-}
nlAddNeighbor :: forall k dp.
    ( ViewParam Param_k (NeighborList k dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> Neighbor dp -> NeighborList k dp
nlAddNeighbor nl n = nl + NL_Cons n NL_Nil

instance CanError (NeighborList k dp) where
    {-# INLINE errorVal #-}
    errorVal = NL_Nil

    {-# INLINE isError #-}
    isError NL_Nil = True
    isError _ = False

instance
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , MetricSpace dp
    , Eq dp
    , ValidNeighbor dp
    ) => Monoid (NeighborList k dp)
        where

    {-# INLINE zero #-}
    zero = NL_Nil

instance
    ( ViewParam Param_k (NeighborList k dp)
    , MetricSpace dp
    , Eq dp
    , ValidNeighbor dp
    ) => Semigroup (NeighborList k dp)
        where

    {-# INLINE (+) #-}
    nl1    + NL_Nil = nl1
    NL_Nil + nl2    = nl2
    nl1    + nl2    = ret
        where
            ret = go nl1 nl2 (viewParam _k nl1)

            go _ _ 0 = NL_Nil
            go (NL_Cons n1 ns1) (NL_Cons n2 ns2) k = if neighborDistance n1 > neighborDistance n2
                then NL_Cons n2 $ go (NL_Cons n1 ns1) ns2 (k-1)
                else NL_Cons n1 $ go ns1 (NL_Cons n2 ns2) (k-1)
            go NL_Nil (NL_Cons n2 ns2) k = NL_Cons n2 $ go NL_Nil ns2 (k-1)
            go (NL_Cons n1 ns1) NL_Nil k = NL_Cons n1 $ go ns1 NL_Nil (k-1)
            go NL_Nil NL_Nil k = NL_Nil

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

newtype NeighborMap (k :: Config Nat) dp = NeighborMap
    { nm2map :: Map' dp (NeighborList k dp)
--     { nm2map :: Seq (dp , NeighborList k dp)
    }
mkParams ''NeighborMap
-- deriveHierarchy ''NeighborMap []

type instance Logic (NeighborMap k dp) = Bool

deriving instance (Eq_ dp, ValidNeighbor dp) => Eq_ (NeighborMap k dp)
deriving instance (Read dp, Read (Scalar dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Scalar dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborMap k dp)

-- deriving instance (Eq_ (Map' dp (NeighborList k dp))) => Eq_ (NeighborMap k dp)
-- deriving instance (Read dp, Read (Scalar dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
-- deriving instance (Show dp, Show (Scalar dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
-- deriving instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborMap k dp)

{-# INLINE nm2list #-}
nm2list ::
    ( Param_k (NeighborList k dp)
    , ValidNeighbor dp
    , Lattice dp
    , Ord dp
    ) => NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = toList nm

instance
    ( ViewParam Param_k (NeighborList k dp)
    , MetricSpace dp
    , Ord dp
    , ValidNeighbor dp
    ) => Monoid (NeighborMap k dp)
        where

    {-# INLINE zero #-}
    zero = NeighborMap zero

instance
    ( ViewParam Param_k (NeighborList k dp)
    , MetricSpace dp
    , Ord dp
    , ValidNeighbor dp
    ) => Semigroup (NeighborMap k dp)
        where
    {-# INLINE (+) #-}
    (NeighborMap x)+(NeighborMap y) =
        {-# SCC mappend_NeighborMap #-} NeighborMap $ x+y

-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => t dp -> dp -> NeighborList k dp
findNeighborList !t !query = findNeighborListWith zero t query

{-# INLINABLE findNeighborListWith #-}
findNeighborListWith ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> t dp -> dp -> NeighborList k dp
findNeighborListWith !nl !t !q = findEpsilonNeighborListWith nl 0 t q

-- {-# INLINABLE findEpsilonNeighborList #-}
-- findEpsilonNeighborList !e !t !q = findEpsilonNeighborListWith zero e t q

{-# INLINABLE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query =
    {-# SCC findEpsilonNeighborListWith #-} prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
    where
        smudge = 1/(1+epsilon)

-- {-# INLINABLE findNeighborList_batch #-}
-- findNeighborList_batch v st = fmap (findNeighborList st) v

{-# INLINABLE knn_catadp #-}
-- {-# INLINE knn_catadp #-}
knn_catadp :: forall k dp.
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , MetricSpace dp
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp #-}
    if dp==query
        then knn
        else if isError dist
            then knn
            else nlAddNeighbor knn $ Neighbor dp dist
--             else knn + nlSingleton (Neighbor dp dist)
    where
        !dist = isFartherThanWithDistanceCanError dp query $ nlMaxDist knn * smudge

{-# INLINABLE knn_cata #-}
-- {-# INLINE knn_cata #-}
knn_cata :: forall k t dp.
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Floating (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-}
    if {-# SCC knn_cata_eq #-} stNode t==query
        then {-# SCC knn_cata_then #-} if {-# SCC knn_cata_isError_knn #-} isError knn
            then nlSingleton (Neighbor (stNode t) maxBound)
            else knn
        else {-# SCC knn_cata_else #-}if {-# SCC knn_cata_isError_dist #-}isError dist
            then errorVal
            else nlAddNeighbor knn $ Neighbor (stNode t) dist
--             else knn + nlSingleton (Neighbor (stNode t) dist)
    where
        dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query $ nlMaxDist knn * smudge

---------------------------------------

{-# INLINABLE findNeighborVec #-}
findNeighborVec ::
    ( SpaceTree t dp
--     , KnownNat k
    , ViewParam Param_k (NeighborList k dp)
    , ValidNeighbor dp
    , Floating (Scalar dp)
    ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
findNeighborVec dual = VG.fromList $ map (findNeighborList $ reference dual) $ stToList $ query dual

-- {-# INLINABLE findNeighborSL #-}
-- findNeighborSL ::
--     ( SpaceTree t dp
-- --     , KnownNat k
--     , ViewParam Param_k (NeighborList k dp)
--     , ValidNeighbor dp
--     ) => DualTree (t dp) -> Strict.List (NeighborList k dp)
-- findNeighborSL dual = Strict.list2strictlist $ map (findNeighborList $ reference dual) $ stToList $ query dual

-- {-# INLINABLE findNeighborVecM #-}
-- findNeighborVecM ::
--     ( SpaceTree t dp
-- --     , KnownNat k
--     , ViewParam Param_k (NeighborList k dp)
--     , ValidNeighbor dp
--     ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
-- findNeighborVecM dual = runST $ do
--     let qvec = V.fromList $ stToList $ query $ dual
--     ret <- VM.new (VG.length qvec)
--
--     let go i = if i >= VG.length qvec
--             then return ()
--             else do
--                 let nl = findNeighborList (reference dual) $ qvec VG.! i
--                 seq nl $ return ()
--                 VM.write ret i nl
--                 go $ i+1
--
--     go 0
--     VG.unsafeFreeze ret

{-# INLINABLE findNeighborVec' #-}
findNeighborVec' ::
    ( SpaceTree t dp
--     , KnownNat k
    , ViewParam Param_k (NeighborList k dp)
    , ValidNeighbor dp
    , Floating (Scalar dp)
    ) => DualTree (t dp) -> V.Vector (NeighborList k dp)
findNeighborVec' dual = V.generate (VG.length qvec) go
    where
        go i = findNeighborList (reference dual) $ qvec VG.! i

        qvec = V.fromList $ stToList $ query $ dual

---------------------------------------

singletonAt k v = singleton (k,v)

{-# INLINABLE findNeighborMap #-}
findNeighborMap ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Ord dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => DualTree (t dp) -> NeighborMap k dp
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $
    map (\dp -> NeighborMap $ singletonAt dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} ({-parallel-} reduce) $
    map (\dp -> NeighborMap $ singletonAt dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindEpsilonNeighborMap #-}
parFindEpsilonNeighborMap ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> DualTree (t dp) -> NeighborMap k dp
parFindEpsilonNeighborMap e d = parFindEpsilonNeighborMapWith zero e d

{-# INLINABLE findAllNeighbors #-}
findAllNeighbors :: forall k dp t.
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> t dp -> [dp] -> Seq (dp,NeighborList k dp)
findAllNeighbors epsilon rtree qs = reduce $ map
    (\dp -> singletonAt dp
        $ findEpsilonNeighborListWith zero epsilon rtree dp)
    qs

{-# INLINABLE parFindEpsilonNeighborMapWith #-}
parFindEpsilonNeighborMapWith :: forall k dp t.
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborMap k dp -> Scalar dp -> DualTree (t dp) -> NeighborMap k dp
-- parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = NeighborMap $ map
--     (\dp -> (dp, findEpsilonNeighborListWith zero epsilon (reference dual) dp) )
--     (stToList $ query dual)

parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = parallel reduce $ map
    (\dp -> NeighborMap
        $ singletonAt dp
        $ findEpsilonNeighborListWith zero epsilon (reference dual) dp)
    (stToList $ query dual)

-- parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = parallel reduce $ map
--     (\dp -> NeighborMap
--         $ singletonAt dp
--         $ findEpsilonNeighborListWith (findWithDefault zero dp nm) epsilon (reference dual) dp)
--     (stToList $ query dual)

-- parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual
--     = parallel reduce
--     $ map (NeighborMap . singleton)
--     $ go prev0 (stToList $ query dual) []
--     where
--         prev0 = zero :: NeighborList k dp
--
--         go prev []     ret = ret
--         go prev (x:xs) ret = go next xs ((x,next):ret)
--             where
--                 next=findEpsilonNeighborListWith zero epsilon (reference dual) x
-- --                 next = findEpsilonNeighborListWith (resetNL x prev) epsilon (reference dual) x
-- --
-- --                 prev' = case prev of
-- --                     (NL_Cons (Neighbor prevx _) NL_Nil) -> if prevx==x then zero else prev
-- --                     otherwise -> zero

