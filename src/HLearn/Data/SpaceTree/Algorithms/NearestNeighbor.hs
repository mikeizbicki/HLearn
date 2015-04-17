{-# LANGUAGE DataKinds,UnboxedTuples,MagicHash,TemplateHaskell,RankNTypes,TupleSections #-}

module HLearn.Data.SpaceTree.Algorithms.NearestNeighbor
    (

    -- * data types
    Neighbor (..)
    , ValidNeighbor (..)

    , NeighborList (..)
    , nlSingleton
    , getknnL
    , nlMaxDist

    , Param_k
    , _k

    -- * functions
    , findAllNeighbors
    , findAllNeighbors'

    , findNeighborList
    , findEpsilonNeighborListWith
    )
    where

import qualified Prelude as P
import Data.Strict.Tuple (Pair(..))

import SubHask
import SubHask.Algebra.Container
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector
import SubHask.Compatibility.Vector.Lebesgue
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.SpaceTree

import Data.Params

-------------------------------------------------------------------------------

data Neighbor dp = Neighbor
    { neighbor         :: !dp
    , neighborDistance :: !(Scalar dp)
--     { neighbor         :: {-#UNPACK#-}!(L2 UnboxedVector Float)
--     , neighborDistance :: {-#UNPACK#-}!Float
    }

type instance Scalar (Neighbor dp) = Bool
type instance Logic (Neighbor dp) = Bool

type ValidNeighbor dp =
    ( Metric dp
    , Bounded (Scalar dp)
    , CanError (Scalar dp)
    , Logic dp~Bool
--     , dp ~ (L2 UnboxedVector Float)
    )

deriving instance (Read dp, Read (Scalar dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance Eq (Scalar dp) => Eq_ (Neighbor dp) where
    {-# INLINE (==) #-}
    a == b = neighborDistance a == neighborDistance b

-- instance Ord (Scalar dp) => Ord (Neighbor dp) where
--     compare a b = compare (neighborDistance a) (neighborDistance b)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf (Neighbor _ _) = ()
--     rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

------------------------------------------------------------------------------

data NeighborList (k :: Config Nat) dp
    = NL_Nil
    | NL_Cons {-#UNPACK#-}!(Neighbor dp) !(NeighborList k dp)
--     | NL_Err

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
--     rnf NL_Err = ()
    rnf (NL_Cons n ns) = ()
--     rnf (NL_Cons n ns) = deepseq n $ rnf ns

instance (ValidNeighbor dp, Eq_ dp) => Eq_ (NeighborList k dp) where
    (NL_Cons x xs) == (NL_Cons y ys) = x==y && xs==ys
    NL_Nil         == NL_Nil         = True
--     NL_Err         == NL_Err         = True
    _              == _              = False

property_orderedNeighborList :: (Logic dp~Bool, Metric dp) => NeighborList k dp -> Bool
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
-- getknnL NL_Err = error "getknnL: NL_Err"

{-# INLINE nlMaxDist #-}
nlMaxDist ::
    ( ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp
nlMaxDist !nl = go nl
    where
        go (NL_Cons n NL_Nil) = neighborDistance n
        go (NL_Cons n ns) = go ns
        go NL_Nil = maxBound
--         go NL_Err = maxBound

instance CanError (NeighborList k dp) where
    {-# INLINE errorVal #-}
    errorVal = NL_Nil
--     errorVal = NL_Err

    {-# INLINE isError #-}
    isError NL_Nil = True
--     isError NL_Err = True
    isError _ = False

instance
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , Metric dp
    , Eq dp
    , ValidNeighbor dp
    ) => Monoid (NeighborList k dp)
        where

    {-# INLINE zero #-}
    zero = NL_Nil

instance
    ( ViewParam Param_k (NeighborList k dp)
    , Metric dp
    , Eq dp
    , ValidNeighbor dp
    ) => Semigroup (NeighborList k dp)
        where

    {-# INLINE (+) #-}
--     nl1    + NL_Err = nl1
--     NL_Err + nl2    = nl2
    nl1    + NL_Nil = nl1
    NL_Nil + nl2    = nl2
    nl1    + nl2    = {-# SCC notNiL #-} ret
        where
            ret = go nl1 nl2 (viewParam _k nl1)

            go _ _ 0 = NL_Nil
            go (NL_Cons n1 ns1) (NL_Cons n2 ns2) k = if neighborDistance n1 > neighborDistance n2
                then NL_Cons n2 $ go (NL_Cons n1 ns1) ns2 (k-1)
                else NL_Cons n1 $ go ns1 (NL_Cons n2 ns2) (k-1)
            go NL_Nil (NL_Cons n2 ns2) k = NL_Cons n2 $ go NL_Nil ns2 (k-1)
            go (NL_Cons n1 ns1) NL_Nil k = NL_Cons n1 $ go ns1 NL_Nil (k-1)
            go NL_Nil NL_Nil k = NL_Nil

{-# INLINE nlAddNeighbor #-}
nlAddNeighbor :: forall k dp.
    ( ViewParam Param_k (NeighborList k dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> Neighbor dp -> NeighborList k dp
-- nlAddNeighbor NL_Nil n' = NL_Cons n' NL_Nil
-- nlAddNeighbor (NL_Cons n NL_Nil) n' = if neighborDistance n' > neighborDistance n
--     then NL_Cons n' NL_Nil
--     else NL_Cons n NL_Nil

nlAddNeighbor !nl !n = {-# SCC nlAddNeighbor #-} nl + NL_Cons n NL_Nil

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
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Eq dp
    , Real (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => t dp -> dp -> NeighborList k dp
findNeighborList !t !query = findEpsilonNeighborListWith zero zero t query

{-# INLINABLE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith ::
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Eq dp
    , Real (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => NeighborList k dp -> Scalar dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query =
    {-# SCC findEpsilonNeighborListWith #-}
--     prunefoldC (knn_catadp smudge query) knn t
--     prunefoldB_CanError_sort query (knn_catadp smudge query) (knn_cata_dist smudge query) knn t
    prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
--     prunefoldD (knn_catadp smudge query) (knn_cata2 smudge query) knn t
    where
        smudge = 1/(1+epsilon)

{-# INLINABLE knn_catadp #-}
-- {-# INLINE knn_catadp #-}
knn_catadp :: forall k dp.
--     ( KnownNat k
    ( ViewParam Param_k (NeighborList k dp)
    , Metric dp
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp #-}
    -- dist==0 is equivalent to query==dp,
    -- but we have to calculate dist anyways so it's faster
    if dist==0 || dist>bound
--     if dist==0 || isError dist
        then knn
        else nlAddNeighbor knn $ Neighbor dp dist
    where
        dist = distanceUB dp query bound
        bound = smudge*nlMaxDist knn
--         dist = isFartherThanWithDistanceCanError dp query
--              $ nlMaxDist knn * smudge

-- {-# INLINABLE knn_cata #-}
{-# INLINE knn_cata #-}
knn_cata :: forall k t dp.
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Real (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-}
    if dist==0
        then if isError knn
            then nlSingleton $ Neighbor (stNode t) maxBound
            else knn
        else if isError dist
            then errorVal
            else nlAddNeighbor knn $ Neighbor (stNode t) dist
    where
        dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query
             $ nlMaxDist knn * smudge

{-# INLINABLE prunefoldB_CanError_sort #-}
prunefoldB_CanError_sort ::
    ( SpaceTree t a
    , ValidNeighbor a
    , b ~ NeighborList k a
    , ClassicalLogic a
    , CanError (Scalar a)
    , Bounded (Scalar a)
    ) =>
    a -> (a -> b -> b) -> (Scalar a -> t a -> b -> b) -> b -> t a -> b
prunefoldB_CanError_sort !query !f1 !f2 !b !t = {-# SCC prunefoldB_CanError_sort #-}
    go ( distance (stNode t) query :!: t ) b
    where
        go !( dist :!: t ) !b =  if isError res
            then b
            else foldr' go b'' children'
                where
                    res = f2 dist t b
                    b'' = foldr' f1 res (stLeaves t)

                    children'
                        = {-# SCC children' #-} qsortHalf (\( d1 :!: _ ) ( d2 :!: _ ) -> compare d2 d1)
                        $  map (\x -> ( stIsMinDistanceDpFartherThanWithDistanceCanError x query maxdist
                                      :!: x ))
--                         $ map (\x -> ( distanceUB (stNode x) query (lambda t+maxdist), x ))
--                         $ map (\x -> ( distance (stNode x) query , x ))
                        $ toList
                        $ stChildren t

                    maxdist = nlMaxDist b''

-- | This is a version of quicksort that only descends on its lower half.
-- That is, it only "approximately" sorts a list.
-- It is modified from http://en.literateprograms.org/Quicksort_%28Haskell%29
{-# INLINABLE qsortHalf #-}
qsortHalf :: (a -> a -> Ordering) -> [a] -> [a]
qsortHalf !cmp !x = {-# SCC qsortHalf #-} go x []
    where
        go [] !y     = y
        go [x] !y    = x:y
        go (x:xs) !y = part xs [] [x] []
            where
                part [] !l !e !g = go l (e ++ g ++ y)
                part (z:zs) !l !e !g = case cmp z x of
                    GT -> part zs l e (z:g)
                    LT -> part zs (z:l) e g
                    EQ -> part zs l (z:e) g

{-# INLINABLE knn_cata_dist #-}
knn_cata_dist :: forall k t dp.
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , Real (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> dp -> Scalar dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata_dist !smudge !query !dist !t !knn = {-# SCC knn_cata #-}
    if dist==0
        then if isError knn
            then nlSingleton $ Neighbor (stNode t) maxBound
            else knn
--         else if dist - lambda t > nlMaxDist knn * smudge -- isError dist
        else if isError dist
            then errorVal
            else nlAddNeighbor knn $ Neighbor (stNode t) dist
--     where
--         dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query
--              $ nlMaxDist knn * smudge

---------------------------------------

{-# INLINABLE findAllNeighbors #-}
findAllNeighbors :: forall k dp t.
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , NFData (Scalar dp)
    , NFData dp
    , Real (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> t dp -> [dp] -> Seq (dp,NeighborList k dp)
findAllNeighbors epsilon rtree qs = reduce $ map
    (\dp -> singleton (dp, findEpsilonNeighborListWith zero epsilon rtree dp))
    qs

{-# INLINABLE findAllNeighbors' #-}
findAllNeighbors' :: forall k dp t.
    ( ViewParam Param_k (NeighborList k dp)
    , SpaceTree t dp
    , NFData (Scalar dp)
    , NFData dp
    , Real (Scalar dp)
    , CanError (Scalar dp)
    , ValidNeighbor dp
    ) => Scalar dp -> t dp -> [dp] -> Seq (Labeled' dp (NeighborList k dp))
findAllNeighbors' epsilon rtree qs = fromList $ map
    (\dp -> mkLabeled' dp $ findEpsilonNeighborListWith zero epsilon rtree dp)
    qs
-- findAllNeighbors' epsilon rtree qs = reduce $ map
--     (\dp -> singleton $ mkLabeled' dp $ findEpsilonNeighborListWith zero epsilon rtree dp)
--     qs

mkLabeled' :: x -> y -> Labeled' x y
mkLabeled' x y = Labeled' x y
