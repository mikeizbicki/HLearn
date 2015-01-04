{-# LANGUAGE DataKinds #-}

module HLearn.Data.SpaceTree.CoverTree
    where

import Debug.Trace
import qualified Data.List as L
import Data.Either
import qualified Prelude as P

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Data.Params

import SubHask
import SubHask.Monad
import SubHask.Algebra.Container
import SubHask.Compatibility.Containers

import HLearn.Data.SpaceTree
import HLearn.Models.Distributions.Univariate.Normal

import Diagrams.Prelude hiding (distance,trace,query,connect,Semigroup,(<>),Scalar,Monoid)
import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG

---------
import System.IO.Unsafe
import Data.IORef

import Control.Concurrent
import Control.Parallel.Strategies


{-# NOINLINE expratIORef #-}
expratIORef = unsafePerformIO $ newIORef (2::Rational)

setexpratIORef :: Rational -> P.IO ()
setexpratIORef r = writeIORef expratIORef r

{-# INLINE exprat #-}
exprat :: Field r => r
exprat = fromRational $ unsafePerformIO $ readIORef expratIORef
-- exprat = 1.3

-------------------------------------------------------------------------------

type CoverTree dp = CoverTree_ (13/10) Array Array dp

data CoverTree_
        ( exprat                :: Frac )
        ( childC                :: * -> * )
        ( leafC                 :: * -> * )
        ( dp                    :: * )
    = Node
        { nodedp                :: !dp
        , nodeWeight            :: !(Scalar dp)
        , level                 :: {-#UNPACK#-}!Int
        , numdp                 :: !(Scalar dp)
        , maxDescendentDistance :: !(Scalar dp)
        , children              :: !(childC (CoverTree_ exprat childC leafC dp))
        , leaves                :: !(leafC dp)
        }

type instance Logic (CoverTree_ exprat childC leafC dp) = Bool

instance ValidCT exprat childC leafC dp => Eq_ (CoverTree_ exprat childC leafC dp) where
    ct1==ct2 = stToList ct1==stToList ct2

deriving instance
    ( Show (childC (CoverTree_ exprat childC leafC dp))
    , Show (leafC dp)
    , Show dp
    , Show (Scalar dp)
    ) => Show (CoverTree_ exprat childC leafC dp)

instance
    ( NFData (childC (CoverTree_ exprat childC leafC dp))
    , NFData (leafC dp)
    , NFData dp
    , NFData (Scalar dp)
    ) => NFData (CoverTree_ exprat childC leafC dp) where
    rnf ct = deepseq (nodedp ct)
           $ deepseq (nodeWeight ct)
           $ deepseq (level ct)
           $ deepseq (numdp ct)
           $ deepseq (maxDescendentDistance ct)
           $ deepseq (children ct)
           $ deepseq (leaves ct)
           $ rnf ()

type instance Scalar (CoverTree_ exprat childC leafC dp) = Scalar dp

-- | This type alias simplifies all our type signatures.
--
-- FIXME:
-- There is a much smaller subset of these that is actually needed,
-- but GHC's type system isn't strong enough to express this subset.
type ValidCT exprat childC leafC dp =
    ( Foldable (childC (CoverTree_ exprat childC leafC dp))
    , Foldable (leafC dp)
    , Unfoldable (childC (CoverTree_ exprat childC leafC dp))
    , Unfoldable (leafC dp)
    , Normed (childC (CoverTree_ exprat childC leafC dp))
    , Normed (leafC dp)
    , Elem (childC (CoverTree_ exprat childC leafC dp)) ~ CoverTree_ exprat childC leafC dp
    , Elem (leafC dp) ~ dp
    , VG.Vector childC (CoverTree_ exprat childC leafC dp)
    , VG.Vector childC (Scalar dp)
    , VG.Vector leafC dp
    , VG.Vector leafC (Scalar dp)
    , MetricSpace dp
    , QuotientField (Scalar dp) Int
    , Floating (Scalar dp)
    , Bounded (Scalar dp)
    , CanError (Scalar dp)
    , Ord dp
    , KnownFrac exprat
    , Logic dp ~ Bool
    , Logic (CoverTree_ exprat childC leafC dp) ~ Bool
    , Logic (Scalar (leafC dp)) ~ Bool
    , NFData (CoverTree_ exprat childC leafC dp)

    -- these constraints come from hlearn-allknn
    , Scalar (leafC dp) ~ Scalar (childC (CoverTree_ exprat childC leafC dp))
    , Elem (childC Bool) ~ Bool
    , Elem (childC (Scalar dp)) ~ Scalar dp
    , Elem (leafC dp) ~ dp
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , NFData (Scalar dp)
    , NFData dp

    -- debugging constraints
    , Show (Scalar dp)
    , Show dp
    , Show (leafC dp)
    , Show (childC (CoverTree_ exprat childC leafC dp))
    , Show (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , Show (Scalar (leafC dp))
    )

instance
    ( ValidCT exprat childC leafC dp
    ) => SpaceTree (CoverTree_ exprat childC leafC) dp
        where

    type ChildContainer (CoverTree_ exprat childC leafC ) = childC
    type LeafContainer (CoverTree_ exprat childC leafC ) = leafC

    {-# INLINE stMinDistanceWithDistance #-}
    {-# INLINE stMaxDistanceWithDistance #-}

    stMinDistanceWithDistance !ct1 !ct2 =
        (# dist-(maxDescendentDistance ct1)-(maxDescendentDistance ct2), dist #)
        where dist = distance (nodedp ct1) (nodedp ct2)

    stMaxDistanceWithDistance !ct1 !ct2 =
        (# dist+(maxDescendentDistance ct1)+(maxDescendentDistance ct2), dist #)
        where dist = distance (nodedp ct1) (nodedp ct2)

    {-# INLINE stMinDistanceDpWithDistance #-}
    {-# INLINE stMaxDistanceDpWithDistance #-}

    stMinDistanceDpWithDistance !ct !dp =
        (# dist - maxDescendentDistance ct, dist #)
        where dist = distance (nodedp ct) dp

    stMaxDistanceDpWithDistance !ct !dp =
        (# dist + maxDescendentDistance ct, dist #)
        where dist = distance (nodedp ct) dp

    {-# INLINE stIsMinDistanceDpFartherThanWithDistanceCanError #-}
    {-# INLINE stIsMaxDistanceDpFartherThanWithDistanceCanError #-}

    stIsMinDistanceDpFartherThanWithDistanceCanError !ct !dp !b =
        isFartherThanWithDistanceCanError (nodedp ct) dp (b+maxDescendentDistance ct)

    stIsMaxDistanceDpFartherThanWithDistanceCanError !ct !dp !b =
        isFartherThanWithDistanceCanError (nodedp ct) dp (b-maxDescendentDistance ct)

    {-# INLINE stMinDistanceDpFromDistance #-}
    {-# INLINE stMaxDistanceDpFromDistance #-}

    stMinDistanceDpFromDistance !ct !dp !dist = dist-maxDescendentDistance ct
    stMaxDistanceDpFromDistance !ct !dp !dist = dist+maxDescendentDistance ct

    {-# INLINE stChildren #-}
    {-# INLINE stNode #-}
    {-# INLINE stLeaves #-}
    {-# INLINE stHasNode #-}
    stChildren  = children
    stLeaves    = leaves
    stNode      = nodedp
    stWeight    = nodeWeight
    stHasNode _ = True

    {-# INLINE ro #-}
    ro _ = 0

    {-# INLINE lambda #-}
    lambda !ct = maxDescendentDistance ct

---------------------------------------

{-# INLINE indicator #-}
indicator :: Ring r => Bool -> r
indicator True = 1
indicator False = 0

-- | returns a measure of how "spread out" descendent points are
{-# INLINABLE ctMaxCoverRatio #-}
ctMaxCoverRatio ::
    ( ValidCT exprat childC leafC dp
    , Scalar (childC (CoverTree_ exprat childC leafC dp)) ~ Scalar (leafC dp)
    ) => CoverTree_ exprat childC leafC dp -> Scalar dp
ctMaxCoverRatio ct = if length (children ct) + length (leaves ct) > 0
    then maximum
        $ (stMaxDescendentDistance ct / coverdist ct)
        : map ctMaxCoverRatio (toList $ children ct)
    else 0

-- | returns a measure of how "spread out" descendent points are
{-# INLINABLE ctAveCoverRatio #-}
ctAveCoverRatio ::
    ( ValidCT exprat childC leafC dp
    , Scalar (childC (CoverTree_ exprat childC leafC dp)) ~ Scalar (leafC dp)
    ) => CoverTree_ exprat childC leafC dp -> Normal (Scalar dp)
ctAveCoverRatio ct = if length (children ct) + length (leaves ct) > 0
    then train1Normal (stMaxDescendentDistance ct / coverdist ct)
       + reduce (map ctAveCoverRatio $ toList $ children ct)
    else zero

-- | counts the number of parents in the tree that have nephews that could be placed under them
{-# INLINABLE ctMovableParents #-}
ctMovableParents ::
    ( ValidCT exprat childC leafC dp
    , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , Integral (Scalar (leafC dp))
    ) => CoverTree_ exprat childC leafC dp -> Int
ctMovableParents ct
    = sum (map movableChildren $ toList $ children ct)
    + sum (map ctMovableParents  $ toList $ children ct)
    where
        movableChildren c
            = sum (map totalParentsOfChildren (                  toList $ children c))
            + sum (map totalParentsOfChildren (map singletonCT $ toList $ leaves   c))
            - (fromIntegral $ toInteger $ length $ children c)
            - (fromIntegral $ toInteger $ length $ leaves c)

        totalParentsOfChildren c
            = sum
            $ map (\p -> indicator $ distance (nodedp c) (nodedp p) <= sepdist ct)
            $ toList (children ct)
           ++ toList (map singletonCT $ toList $ leaves ct)

-- | counts the number of parents in the tree that have nephews that would be better placed under them
{-# INLINABLE ctBetterMovableParents #-}
ctBetterMovableParents ::
    ( ValidCT exprat childC leafC dp
    , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , Integral (Scalar (leafC dp))
    ) => CoverTree_ exprat childC leafC dp -> Int
ctBetterMovableParents ct
    = sum (map betterMovableChildren  $ toList $ children ct)
    + sum (map ctBetterMovableParents $ toList $ children ct)
    where
        betterMovableChildren c
            = sum (map (totalBetterParentsOfChildren c) (                  toList $ children c))
            + sum (map (totalBetterParentsOfChildren c) (map singletonCT $ toList $ leaves   c))
            - (fromIntegral $ toInteger $ length $ children c)
            - (fromIntegral $ toInteger $ length $ leaves c)

        totalBetterParentsOfChildren realparent c
            = sum
            $ map (\p -> indicator $ distance (nodedp c) (nodedp p)
                                  <= distance (nodedp c) (nodedp realparent))
            $ toList (children ct)
           ++ toList (map singletonCT $ toList $ leaves ct)

-- | counts the number of nodes in the tree with uncles they could be moved under
{-# INLINABLE ctMovableNodes #-}
ctMovableNodes ::
    ( ValidCT exprat childC leafC dp
    , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , Integral (Scalar (leafC dp))
    ) => CoverTree_ exprat childC leafC dp -> Int
ctMovableNodes ct
    = sum (map movableChildren $ toList $ children ct)
    + sum (map ctMovableNodes  $ toList $ children ct)
    where
        movableChildren c
            = sum (map totalNodesOfChildren (                  toList $ children c))
            + sum (map totalNodesOfChildren (map singletonCT $ toList $ leaves   c))
--             - (fromIntegral $ toInteger $ length $ children c)
--             - (fromIntegral $ toInteger $ length $ leaves c)

        totalNodesOfChildren c
            = indicator
            $ (> (1::Int))
            $ sum
            $ map (\p -> indicator $ distance (nodedp c) (nodedp p) < sepdist ct)
            $ toList (children ct)
           ++ toList (map singletonCT $ toList $ leaves ct)

-- | counts the number of nodes in the tree with uncles they would be better placed under
{-# INLINABLE ctBetterMovableNodes #-}
ctBetterMovableNodes ::
    ( ValidCT exprat childC leafC dp
    , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
    , Integral (Scalar (leafC dp))
    ) => CoverTree_ exprat childC leafC dp -> Int
ctBetterMovableNodes ct
    = sum (map betterMovableChildren  $ toList $ children ct)
    + sum (map ctBetterMovableNodes $ toList $ children ct)
    where
        betterMovableChildren c
            = sum (map (totalBetterNodesOfChildren c) (                  toList $ children c))
            + sum (map (totalBetterNodesOfChildren c) (map singletonCT $ toList $ leaves   c))
--             - (fromIntegral $ toInteger $ length $ children c)
--             - (fromIntegral $ toInteger $ length $ leaves c)

        totalBetterNodesOfChildren realparent c
            = indicator
            $ or
            $ map (\p -> if distance (nodedp c) (nodedp p)
                          < distance (nodedp c) (nodedp realparent)
--                     then trace ("  distance c p = "++show (distance (nodedp c) (nodedp p))
--                               ++"; distance c realparent = "++show (distance (nodedp c) (nodedp realparent))
--                               ++"; c = "++show (nodedp c)
--                               ++"; p = "++show (nodedp p)
--                               ++"; realparent = "++show (nodedp c)
--                               )
                    then --trace ("  ct = "++show ct)
                               True
                    else False
                          )
            $ toList (children ct)
           ++ toList (map singletonCT $ toList $ leaves ct)

-------------------------------------------------------------------------------
-- tests

invariant_CoverTree_covering ::
    ( ValidCT exprat childC leafC dp
    , Elem (childC (Scalar dp)) ~ Scalar dp
    , Elem (childC Bool) ~ Bool
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    , VG.Vector childC Bool
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_covering node
    = and (map invariant_CoverTree_covering $ stChildrenList node)
--    && and (map (\child -> distance  (nodedp node) (nodedp child) <= 1.1*coverdist node) $ stChildrenList node)
   && and (map (\child -> distance  (nodedp node) (nodedp child) <= coverdist node) $ stChildrenList node)

invariant_CoverTree_tightCovering ::
    ( ValidCT exprat childC leafC dp
    , Elem (childC (Scalar dp)) ~ Scalar dp
    , Elem (childC Bool) ~ Bool
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    , VG.Vector childC Bool
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_tightCovering node
    = and (map invariant_CoverTree_tightCovering $ stChildrenList node)
   && and (map (\dp -> distance dp (nodedp node) <= coverdist node) $ stDescendents node)

invariant_CoverTree_maxDescendentDistance ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_maxDescendentDistance node
    = and (map invariant_CoverTree_maxDescendentDistance $ stChildrenList node)
--    && and (map (\dp -> distance dp (nodedp node) <= 1.1 * maxDescendentDistance node) $ stDescendents node)
   && and (map (\dp -> distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

invariant_CoverTree_separating ::
    ( ValidCT exprat childC leafC dp
    , Elem (childC Bool) ~ Bool
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    , VG.Vector childC Bool
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_separating node
    = minimum ( P.filter (>0)
              $ map (\(x,y) -> distance (nodedp x) (nodedp y))
              $ cartesianProduct (toList $ children node) (toList $ children node)
              )
      >= 0.9*sepdist node
--       >= sepdist node
   && and (VG.map invariant_CoverTree_separating $ children node)
    where
        cartesianProduct :: [a] -> [b] -> [(a,b)]
        cartesianProduct xs ys = P.concatMap (\y -> map (\x -> (x,y)) xs) ys

-- | FIXME: is this needed?
property_leveled ::
    ( ValidCT exprat childC leafC dp
    , VG.Vector childC Bool
    , VG.Vector childC Int
    ) => CoverTree_ exprat childC leafC dp -> Bool
property_leveled node
    = VG.all (== VG.head xs) xs
   && VG.and (VG.map property_leveled $ children node)
    where
        xs = VG.map level $ children node

-------------------------------------------------------------------------------
-- optimization helpers

-- | uncles adopt all children that are closer to them than their parents
{-# INLINABLE ctAdoptNodes #-}
ctAdoptNodes ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
ctAdoptNodes ct = ct { children = fromList $ map ctAdoptNodes $ go [] $ toList $ children ct }
    where
        go acc []     = acc
--         go acc (x:xs) = go ((foldl' (+) x (accchildren++xschildren)):acc') xs'
        go acc (x:xs) = go (x { children = (fromList accchildren+fromList xschildren+children x)}:acc') xs'
            where
                betterMoveNode y c = distance (nodedp x) (nodedp c) < distance (nodedp y) (nodedp c)

                extractChildren y = ( oldchildren, y { children = fromList newchildren } )
                    where
                        (oldchildren,newchildren) = L.partition (betterMoveNode y) $ toList $ children y

                extractChildrenL ys = foldl' ecgo ([],[]) ys
                    where
                        ecgo (oldchildL, newchildL) y = (oldchildren++oldchildL,newparent:newchildL)
                            where
                                (oldchildren, newparent) = extractChildren y

                (accchildren, acc') = extractChildrenL acc
                (xschildren, xs') = extractChildrenL xs

-- ctMovableNodes ::
--     ( ValidCT exprat childC leafC dp
--     , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
--     , Integral (Scalar (leafC dp))
--     ) => CoverTree_ exprat childC leafC dp -> Int
-- ctMovableNodes ct
--     = sum (map movableChildren $ toList $ children ct)
--     + sum (map ctMovableNodes  $ toList $ children ct)
--     where
--         movableChildren c
--             = sum (map totalNodesOfChildren (                  toList $ children c))
--             + sum (map totalNodesOfChildren (map singletonCT $ toList $ leaves   c))
-- --             - (fromIntegral $ toInteger $ length $ children c)
-- --             - (fromIntegral $ toInteger $ length $ leaves c)
--
--         totalNodesOfChildren c
--             = indicator
--             $ (> (1::Int))
--             $ sum
--             $ map (\p -> indicator $ distance (nodedp c) (nodedp p) < sepdist ct)
--             $ toList (children ct)
--            ++ toList (map singletonCT $ toList $ leaves ct)

{-# INLINABLE setMaxDescendentDistance #-}
setMaxDescendentDistance ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
setMaxDescendentDistance ct = ct
    { children = children'
    , maxDescendentDistance = maximum $ map (\dp -> distance dp (nodedp ct)) $ stDescendents ct
    }
    where
        children' = VG.map setMaxDescendentDistance $ children ct
--         distanceL = map (\dp -> distance (nodedp ct) dp) $ stDescendents ct

---------------------------------------

{-# INLINABLE sortChildren #-}
sortChildren ::
    ( ValidCT exprat childC leafC dp
    ) => ( CoverTree_ exprat childC leafC dp
        -> CoverTree_ exprat childC leafC dp
        -> CoverTree_ exprat childC leafC dp
        -> Ordering
         )
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
sortChildren cmp ct = ct
    { children = fromList $ L.sortBy (cmp ct) $ map (sortChildren cmp) $ toList $ children ct
    }

cmp_numdp_distance ct a b
    = compare (numdp a) (numdp b)
    + compare (distance (nodedp ct) (nodedp a)) (distance (nodedp ct) (nodedp b))

cmp_numdp_distance' ct b a
    = compare (numdp a) (numdp b)
    + compare (distance (nodedp ct) (nodedp a)) (distance (nodedp ct) (nodedp b))

cmp_distance_numdp ct a b
    = compare (distance (nodedp ct) (nodedp a)) (distance (nodedp ct) (nodedp b))
    + compare (numdp a) (numdp b)

cmp_distance_numdp' ct b a
    = compare (distance (nodedp ct) (nodedp a)) (distance (nodedp ct) (nodedp b))
    + compare (numdp a) (numdp b)

---------------------------------------

-- | This function moves subtrees that have @n@ children or less from @childC@ to @leafC@.
-- The @leafC@ container stores data points directly.
-- This makes traversals faster and greatly improves runtimes.
{-# INLINABLE setLeaves #-}
setLeaves ::
    ( ValidCT exprat childC leafC dp
    ) => Int
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
setLeaves n ct = {-# SCC setLeaves #-} if stNumNodes ct > n
    then ct
        { children = fromList $ map (setLeaves n) $ L.filter (not . stHasNoChildren) $ toList $ children ct
        , leaves = fromList $ map nodedp $ L.filter stHasNoChildren $ toList $ children ct
        }
    else ct
        { children = zero
        , leaves = fromList $ stToList ct
        }

-- | This function puts the data points into a van Embde Boas layout.
-- This is a cache oblivious layout that causes queries to have fewer cache misses (and hence run faster).
-- Any modifications to the cover tree after calling "PackCT" will result in a slow degradation of cache performance.
{-# INLINABLE packCT #-}
packCT ::
    ( ValidCT exprat childC leafC dp
    , VG.Vector leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
packCT ct = {-# SCC packCT #-} snd $ go 0 ct
    where
        dpvec = fromList $ stToList ct

        go !i !t = {-# SCC packCT_go #-} ( i',t
            { nodedp = dpvec VG.! i
            , leaves = VG.slice (i+1) (VG.length $ leaves t) dpvec
            , children = fromList children'
            } )
            where
                (i',children') = {-# SCC mapAccumL #-} L.mapAccumL
                    go
                    (i+1+length (toList $ leaves t))
                    (toList $ children t)

-------------------------------------------------------------------------------

-- FIXME: add proper container hierarchy
type instance Elem (CoverTree_ exprat childC leafC dp) = dp

instance
    ( ValidCT exprat childC leafC dp
    ) => Container (CoverTree_ exprat childC leafC dp)
        where

    -- FIXME: use the covertree's structure!
    elem e ct = elem e $ stToList ct
    notElem = not elem

instance
    ( ValidCT exprat childC leafC dp
    ) => Unfoldable (CoverTree_ exprat childC leafC dp)
        where

    singleton = singletonCT

instance
    ( ValidCT exprat childC leafC dp
    ) => Foldable (CoverTree_ exprat childC leafC dp)
        where
    toList = stToList

instance
    ( ValidCT exprat childC leafC dp
    ) => Monoid (CoverTree_ exprat childC leafC dp)

-------------------------------------------------------------------------------
-- construction

{-# INLINABLE trainInsert #-}
trainInsert ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => AddChildMethod (Elem xs)
      -> xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsert addChild xs = {-# SCC trainInsert #-} case unCons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ foldr' (insertCT addChild) (singletonCT dp) $ toList dps

{-# INLINABLE trainInsertNoSort #-}
trainInsertNoSort ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsertNoSort xs = {-# SCC trainInsertNoSort #-}case unCons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ foldr' insertCTNoSort (singletonCT dp) dps

{-# INLINABLE trainMonoid #-}
trainMonoid ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainMonoid xs = {-# SCC trainMonoid #-} foldtree1 $ map (Just' . singletonCT) $ toList xs

{-# INLINABLE singletonCT #-}
singletonCT :: ValidCT exprat childC leafC dp => dp -> CoverTree_  exprat childC leafC dp
singletonCT dp = Node
    { nodedp                = dp
    , nodeWeight            = 1
    , level                 = minBound
    , numdp                 = 1
    , children              = empty
    , leaves                = empty
    , maxDescendentDistance = 0
    }

{-# INLINABLE insertCTNoSort #-}
insertCTNoSort :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCTNoSort dp ct = {-# SCC insertCTNoSort #-}
    if dist > coverdist ct

        -- | ct can't cover dp, so create a new node at dp that covers ct
        then Node
            { nodedp                = dp
            , nodeWeight            = 1
            , level                 = dist2level_up (Proxy::Proxy exprat) dist
            , numdp                 = numdp ct+1
            , maxDescendentDistance = dist+maxDescendentDistance ct
            , children              = singleton
                                    $ raiseRootLevel (dist2level_down (Proxy::Proxy exprat) dist)
                                    $ ct
            , leaves                = empty
            }

        -- | insert dp underneath ct
        else ct
            { numdp                 = numdp ct+1
            , maxDescendentDistance = max dist (maxDescendentDistance ct)
            , children              = fromList $ go [] $ toList $ children ct
            }

        where
            dist = distance (nodedp ct) dp

            go !acc []     = ((singletonCT dp) { level = level ct-1 }):acc
            go !acc (x:xs) = if isFartherThan (nodedp x) dp (sepdist ct)
                then go (x:acc) xs
                else acc+((insertCTNoSort dp x):xs)

-- |
--
-- FIXME:
-- This function does a few more distance calls than "insertCTNoSort",
-- but runs WAY slower.
-- I'm pretty sure the intermediate list of children isn't getting fused away for some reason.
{-# INLINABLE insertCT #-}
insertCT :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCT addChild dp ct =
    insertCT_ addChild dp ct (distance dp $ nodedp ct)

{-# INLINABLE insertCT_ #-}
insertCT_ :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> Scalar dp
      -> CoverTree_ exprat childC leafC dp
insertCT_ addChild dp ct dist = {-# SCC insertCT_ #-}
    if dist > coverdist ct
        -- | ct can't cover dp, so create a new node at dp that covers ct
        then {-# SCC insertCT_greater #-} Node
            { nodedp                = dp
            , nodeWeight            = 1
            , level                 = dist2level_up (Proxy::Proxy exprat) dist
            , numdp                 = numdp ct+1
            , maxDescendentDistance = dist+maxDescendentDistance ct
            , children              = singleton
                                    $ raiseRootLevel (dist2level_down (Proxy::Proxy exprat) dist)
                                    $ ct
            , leaves                = empty
            }

        -- | insert dp underneath ct
        else {-# SCC insertCT_under #-}ct
            { numdp                 = numdp ct+1
            , maxDescendentDistance = max dist (maxDescendentDistance ct)
            , children              = fromList $ go [] childrendists
            }

        where
            childrendists = {-# SCC childrendists #-}
                map (\x -> (distance dp (nodedp x), x)) $ toList $ children ct

            mindist = {-# SCC mindist #-} minimum $ map fst childrendists

            go !acc [] = {-# SCC go_addChild #-} addChild dp ct
            go !acc ((dist,x):xs) = if dist == mindist && dist <= coverdist x
                then insertCT_ addChild dp x dist:(acc+map snd xs)
                else go (x:acc) xs

-- |
--
-- precondition: dp satisfies the covering and separating conditions
--
-- returns: the update list of children for ct
--
type AddChildMethod dp = forall exprat childC leafC.
    ValidCT exprat childC leafC dp
        => dp
        -> CoverTree_ exprat childC leafC dp
        -> [CoverTree_ exprat childC leafC dp]

{-# INLINABLE addChild_nothing #-}
addChild_nothing :: AddChildMethod dp
addChild_nothing dp ct = cons
    ( (singletonCT dp) { level = level ct-1} )
    (toList $ children ct)

{-# INLINABLE addChild_ancestor #-}
addChild_ancestor :: AddChildMethod dp
addChild_ancestor dp ct = toList $ children $ foldr' (insertCT addChild_ancestor)  ct' dps
    where
        (acc',dps) = rmCloseChildren addChild_ancestor dp (sepdist ct) $ toList $ children ct
        ct' = ct
            { children = ((singletonCT dp) { level = level ct-1 }) `cons` fromList acc'
            }

{-# INLINABLE addChild_parent #-}
addChild_parent :: AddChildMethod dp
addChild_parent dp ct = ret:acc'
    where
        ret=foldr' (insertCT addChild_parent) ((singletonCT dp) { level = level ct-1 }) dps
        (acc',dps) = rmCloseChildren addChild_parent dp (sepdist ct) $ toList $ children ct

{-# INLINABLE rmCloseChildren #-}
rmCloseChildren ::
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod dp
      -> dp
      -> Scalar dp
      -> [CoverTree_ exprat childC leafC dp]
      -> ([CoverTree_ exprat childC leafC dp], [dp])
rmCloseChildren addChild dp maxdist cts = {-# SCC rmCloseChildren #-}
    (map fst xs, P.concat $ map snd xs)
    where
        xs = map (extractCloseChildren addChild dp maxdist) cts

{-# INLINABLE extractCloseChildren #-}
extractCloseChildren :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod dp
      -> dp
      -> Scalar dp
      -> CoverTree_ exprat childC leafC dp
      -> (CoverTree_ exprat childC leafC dp, [dp])
extractCloseChildren addChild dp maxdist root = {-# SCC extractCloseChildren #-}
    case go root of
        Right x -> x
    where
        go :: CoverTree_ exprat childC leafC dp
           -> Either ([dp],[dp]) (CoverTree_ exprat childC leafC dp, [dp])
        go ct = if distance (nodedp ct) (nodedp root) > distance (nodedp ct) dp
            then let (valid,invalid) = L.partition (distance (nodedp root) < distance dp)
                                     $ stDescendents ct
                in Left (valid,nodedp ct:invalid)

            else Right
                ( foldr' (insertCT addChild) ct' newbabies
                , invalids+P.concat (map snd $ rights allbabies)
                )
                where
                    ct' = ct
                        { children = fromList $ map fst $ rights allbabies
                        }

                    allbabies = map go $ toList $ children ct
                    (newbabies,invalids) = foldl' (+) ([],[]) $ lefts allbabies

instance
    ( ValidCT exprat childC leafC dp
    ) => Abelian (CoverTree_ exprat childC leafC dp)

instance
    ( ValidCT exprat childC leafC dp
    ) => Semigroup (CoverTree_ exprat childC leafC dp)
        where

    {-# INLINABLE (+) #-}
    ct1 + ct2 = {-# SCC semigroup_CoverTree #-} case ctmerge_ ct1_ ct2_ dist of
        (ct, []) -> ct
        (ct, xs) -> foldr' insertCTNoSort ct $ concat $ map stToList xs
--         (ct, xs) -> foldr' (insertCT addChild_nothing) ct $ concat $ map stToList xs

        where
            dist = distance (nodedp ct1) (nodedp ct2)

            maxlevel = maximum
                [ level ct1
                , level ct2
                , dist2level_down (Proxy::Proxy exprat) dist
                ]

            ct1_ = if level ct1 < maxlevel then raiseRootLevel maxlevel ct1 else ct1
            ct2_ = if level ct2 < maxlevel then raiseRootLevel maxlevel ct2 else ct2

{-
    ct1 + ct2 = {-# SCC semigroup_CoverTree #-}
--         trace "semigroup" $
        if level ct1_ == level ct2_
            then {-# SCC plus_then #-} {-trace " then" $ -} case ctmerge_ ct1_ ct2_ dist of
                (ct, []) -> ct
                (ct, xs) -> {-trace ("  |xs|="++show (length xs)) $-} foldr' (insertCT addChild_nothing) ct $ concat $ map stToList xs
            else {-# SCC plus_else #-} {-trace " else" $ -} ct1_
                { children = fromList $ go [] (stChildrenList ct1_)
                , numdp = numdp ct1_ + numdp ct2_
                , maxDescendentDistance = maxDescendentDistance ct2_ + dist
                }

        where
            go ret []     = (raiseRootLevel (maxlevel-1) ct2_):ret
            go ret (x:xs) = if isFartherThan (nodedp x) (nodedp ct2_) (coverdist x)
                then go (x:ret) xs
                else (x+ct2_):(ret++xs)

            dist = distance (nodedp ct1) (nodedp ct2)

            maxlevel = maximum
                [ level ct1
                , level ct2
                , dist2level_down (Proxy::Proxy exprat) dist
                ]

            (ct1_,ct2_) = if level ct1 >= level ct2
                then (raiseRootLevel maxlevel ct1,ct2)
                else (raiseRootLevel maxlevel ct2,ct1)
-}

-- | If the following prerequisites are met:
--
-- > level ct1==level ct2
--
-- > distance (nodedp ct1) (nodedp ct2) < coverdist ct1
--
-- then all output covertrees will be valid.
-- The root not of ct1 is guaranteed to not change.
{-# INLINABLE ctmerge_ #-}
ctmerge_ :: forall exprat childC leafC  dp.
    ( ValidCT exprat childC leafC  dp
    ) => CoverTree_ exprat childC leafC  dp
      -> CoverTree_ exprat childC leafC  dp
      -> Scalar dp
      -> ( CoverTree_ exprat childC leafC  dp
         , [CoverTree_ exprat childC leafC  dp]
         )
ctmerge_ ct1 ct2 dist =
--   assert "ctmerge_ level  ==" (level ct1==level ct2) $
--   assert "ctmerge_ covdist <" (distance (nodedp ct1) (nodedp ct2) <= coverdist ct1) $

    {-# SCC ctmerge_ #-}
    ( ct' , notCoveredChildren+leftovers' )
    where
        -- remove the children of ct2 that can't be covered by ct1 ("notCoveredChildren");
        -- these will be handled separately in (+)
        (coveredChildren,notCoveredChildren) = {-# SCC isCovered #-}
            L.partition isCovered $ stChildrenList ct2
            where
                isCovered x = not $ isFartherThan (nodedp ct1) (nodedp x) (coverdist ct1)

        -- sepcovChildren is not a part of children' because we loop through children' in go
        -- adding in sepcovChildren would result in extra iterations
        (children',sepcovChildren,leftovers)
            = foldl' (go []) (stChildrenList ct1,[],[]) coveredChildren
            where
                -- merge covChild with a child of ct1' within the separating distance
                go tot (x:xs,ys,zs) covChild = if godist <= sepdist ct1
                    then let (x',zs') = ctmerge_ x covChild godist
                         in (x':tot++xs,ys,zs'++zs)
                    else go (x:tot) (xs,ys,zs) covChild
                    where
                        godist = distance (nodedp x) (nodedp covChild)

                -- nothing within separating distance, so add covChild as new child
                go tot ([],ys,zs) covChild = (tot,covChild:ys,zs)

        -- update children, insert root of ct2 into ct1
        ct_minusleftovers = insertCTNoSort
--         ct_minusleftovers = insertCT_
--             addChild_nothing
            (nodedp ct2)
            ( ct1
                { children = fromList $ children' + sepcovChildren
                , numdp =  sum $ map numdp $ children' + sepcovChildren
                , maxDescendentDistance = coverdist ct1*2
                } )
--             dist

        -- insert any leftovers into the tree if they fit
        (ct',leftovers') = foldl' go (ct_minusleftovers,[]) leftovers
            where
                go (ct,xs) x = if level ct >= level x && godist <= coverdist ct
                    then (goct, goleftovers++xs)
                    else (ct,x:xs)
                    where
                        godist = distance (nodedp ct) (nodedp x)
                        (goct,goleftovers) = ctmerge_ ct (raiseRootLevel (level ct) x) godist


        {-
        -- all covered children of ct2 that don't violate the separating constraint of ct1
        -- can be directly added to ct1
        (sepcovChildren,notSepcovChildren) = {-# SCC isSeparated #-}L.partition isSeparated $ coveredChildren
            where
                isSeparated x = and
                    $ map (\y -> isFartherThan (nodedp y) (nodedp x) (sepdist ct1))
                    $ stChildrenList ct1

        -- covered children that are not separated must be merged with a close neighbor from ct1
        (children',leftovers) = {-# SCC foldgo #-} foldl' (go []) (stChildrenList ct1,[]) notSepcovChildren
            where
                go _   ([],  []) _           = error "this should never happen"
                go tot (x:xs,ys) sepcovChild = if distance (nodedp x) (nodedp sepcovChild) < coverdist x
                    then let (x',ys') = ctmerge_ x sepcovChild
                         in (x':tot++xs,ys'++ys)
                    else go (x:tot) (xs,ys) sepcovChild
-}

--   trace (" ctmerge_ (stNumDp) "++ show (stNumDp ct1) ++ " " ++ show (stNumDp ct2)) $
--   trace (" ctmerge_ (level)   "++ show (level   ct1) ++ " " ++ show (level   ct2)) $
--
--   trace (" |leftovers|="+show (sum $ map numdp $ leftovers)) $
--   trace ("  |coveredChildren|="+show (sum $ map numdp $ coveredChildren)) $
--   trace ("  |notCoveredChildren|="+show (sum $ map numdp $  notCoveredChildren)) $
--   trace ("  |sepcovChildren|="+show (sum $ map numdp $  sepcovChildren)) $
--   trace ("  |notSepcovChildren|="+show (sum $ map numdp $  notSepcovChildren)) $
--   trace (" |children'|="+show (sum $ map numdp $  children')) $
--   trace (" |children ct1|="+show (sum $ map numdp $ toList $ children ct1)) $
--   trace (" |children ct2|="+show (sum $ map numdp $ toList $ children ct2)) $
--   assert
--     ("ctmerge_ debug_"
--         ++"\n children'="++show children'
--         ++"\n notCoveredChildren="++show notCoveredChildren
--         ++"\n notSepcovChildren="++show notSepcovChildren
--         ++"\n children ct1="++show (stChildrenList ct1)
--         ++"\n children ct2="++show (stChildrenList ct2)
--     )
--     (sum (map stNumDp $ notCoveredChildren + leftovers + toList children' + sepcovChildren) +2
--     == stNumDp ct1 + stNumDp ct2
--     ) $
{-
        children' =
            fromList (values newchildren + valid_newleftovers)

        leftovers = invalidchildren++invalid_newleftovers

        -- | FIXME: replace go with fold, better variable names
        -- FIXME: if CT is an IndexedVector, we can get rid of the map
        (separatedChildren,newleftovers) =
--           trace (" |coveredChildren|="++show (length coveredChildren)) $
--           trace (" |notCoveredChildren|="++show (length notCoveredChildren)) $
            go (ct2map' ct1,[]) coveredChildren
        (valid_newleftovers,invalid_newleftovers) =
--           trace (" |newleftovers="++show (length newleftovers)) $
            L.partition validchild newleftovers

        go (childmap,leftovers) []     =
--           trace ("  go: 0") $
--           trace ("   |childmap|="++show (length childmap)) $
--           trace ("   |leftovers|="++show (length leftovers)) $
            (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = {-# SCC ctmerge__go #-}
--           trace ("  go: "++show (length xs+1)) $
            go ( insert (nodedp new, new { level = level ct1-1 }) childmap
               , leftovers'
               ) xs
            where
                (new,leftovers') =  case L.filter (\v -> isFartherThan (nodedp v) (nodedp x) (coverdist v)) $ values childmap of
                    [] -> (x,leftovers)
                    y:_ -> let (new,ys) = ctmerge_ y x in (new,ys+leftovers)
-}
{-
instance
    ( ValidCT exprat childC leafC dp
    ) => Semigroup (CoverTree_ exprat childC leafC dp)
        where
    {-# INLINABLE (+) #-}
    ct1 + ct2 = {-# SCC semigroup #-} case ctmerge_ ct1' ct2' of
        Just' (ct, []) -> ct
        Just' (ct, xs) -> foldl' (+) ct xs
        Nothing' ->
            (raiseRootLevel
                (dist2level_up (Proxy::Proxy exprat) $ distance (nodedp ct1') (nodedp ct2'))
                ct1'
            ) + ct2'
        where
            ct1' = raiseRootLevel maxlevel ct1
            ct2' = raiseRootLevel maxlevel ct2
            maxlevel = {-# SCC maxlevel #-} maximum
                [ level ct1
                , level ct2
                , dist2level_down (Proxy::Proxy exprat) $ distance (nodedp ct1) (nodedp ct2)
                ]

ctmerge_ :: forall exprat childC leafC  dp.
    ( ValidCT exprat childC leafC  dp
    ) => CoverTree_ exprat childC leafC  dp
      -> CoverTree_ exprat childC leafC  dp
      -> Maybe'
            ( CoverTree_ exprat childC leafC  dp
            , [CoverTree_ exprat childC leafC  dp]
            )
ctmerge_ ct1 ct2 = {-# SCC ctmerge_ #-}
--     if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    if isFartherThan (nodedp ct1) (nodedp ct2) (coverdist ct1)
        then Nothing'
        else Just'
            ( insertCT (nodedp ct2) $ ct1
                { children = children'
                , numdp = sum $ map numdp $ toList children'
                , maxDescendentDistance = coverdist ct1
                }
            , invalidchildren++invalid_newleftovers
            )
    where
        children' = fromList $ elems childrenMap'

        childrenMap' = newchildren
                     + fromIndexedList (map (\x -> (nodedp x,raiseRootLevel (level ct1-1) x)) valid_newleftovers)

        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (coverdist ct1)
        (validchildren,invalidchildren) = L.partition validchild $ stChildrenList ct2

        (newchildren,newleftovers) = go (ct2map' ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = L.partition validchild newleftovers

        go (childmap,leftovers) []     = (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = {-# SCC ctmerge__go #-}
            case
                L.filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge_' v x)) $ {-Map.assocs-}toIndexedList childmap of
--                 L.filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge_' v x)) $ {-Map.assocs-}toIndexedList childmap of
                    [] -> go
                        ( {-Map.-}insertAt (nodedp x) (x { level = level ct1-1 }) childmap
                        , leftovers
                        ) xs
--
                    (old, Just' (new,leftovers')):ys ->
                        go ( {-Map.-}insertAt (nodedp new) (new { level = level ct1-1 })
                             $ {-Map.-}deleteAt old childmap
                           , leftovers'++leftovers
                           ) xs
            where
                ctmerge_' ct1 ct2 = {-# SCC ctmerge_' #-} ctmerge_ ct1 ct2
                    where
                        ct1' = raiseRootLevel maxlevel ct1
                        ct2' = raiseRootLevel maxlevel ct2
                        maxlevel = maximum
                            [ level ct1
                            , level ct2
                            , dist2level_down (Proxy::Proxy exprat) $ distance (nodedp ct1) (nodedp ct2)
                            ]

-}

ct2map' ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> Map' dp (CoverTree_ exprat childC leafC dp)
ct2map' ct = fromList $ map (\v -> (nodedp v,v)) $ stChildrenList ct

head x = ret
    where
        (Just ret) = headMaybe x

tail x = ret
    where
        (Just ret) = tailMaybe x

-- | FIXME: does this work if there are leaves present?
{-# INLINABLE raiseRootLevel #-}
raiseRootLevel :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => Int
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
raiseRootLevel i ct
    | stHasNoChildren ct = {-# SCC raiseRootLevel #-}ct { level = i }
    | i <  level ct = error "raiseRootLevel: target level less than current tree level"
    | i == level ct = {-# SCC raiseRootLevel #-}ct
    | i >  level ct = {-# SCC raiseRootLevel #-} raiseRootLevel i $ leaf
        { level = level ct'+1
        , numdp = numdp ct'+nodeWeight leaf
        , maxDescendentDistance = distance (nodedp leaf) (nodedp ct) + maxDescendentDistance ct'
        , children = singleton ct'
        }
    where
        (leaf,ct') = rmleaf ct

{-# INLINABLE rmleaf #-}
rmleaf ::
    ( ValidCT exprat childC leafC  dp
    ) => CoverTree_ exprat childC leafC  dp
      -> ( CoverTree_ exprat childC leafC  dp
         , CoverTree_ exprat childC leafC  dp
         )
rmleaf ct = {-# SCC rmleaf #-} if stHasNoChildren (head childL)
    then ( head childL
         , ct
            { numdp    = numdp ct-nodeWeight (head childL)
            , children = tail childL
            }
         )
    else ( itrleaf
         , ct
            { numdp    = numdp ct-nodeWeight (head childL)
            , children = itrtree `cons` tail childL
            }
         )
    where
        (itrleaf,itrtree) = rmleaf $ head childL
        childL = children ct

---------------------------------------

coverdist :: (QuotientField (Scalar dp) Int, Floating (Scalar dp)) => CoverTree_ exprat childC leafC  dp -> Scalar dp
coverdist node = sepdist node*exprat

sepdist :: forall exprat childC leafC dp. (QuotientField (Scalar dp) Int, Floating (Scalar dp)) =>
    CoverTree_ exprat childC leafC dp -> Scalar dp
sepdist ct = exprat**(fromIntegral $ level ct)

level2sepdist :: (QuotientField f r, Floating f) => r -> f
level2sepdist i = exprat**(fromIntegral i)

level2coverdist :: (QuotientField f r, Floating f) => r -> f
level2coverdist i = level2sepdist (i+1)

dist2level_down :: forall exprat num.
    (KnownFrac exprat, Floating num, QuotientField num Int) => Proxy exprat -> num -> Int
dist2level_down _ d = floor $ log d / log exprat

dist2level_up :: forall exprat num.
    (KnownFrac exprat, Floating num, QuotientField num Int) => Proxy exprat -> num -> Int
dist2level_up _ d = ceiling $ log d / log exprat


-------------------------------------------------------------------------------

-- drawCT ::
--     ( ValidCT exprat childC leafC dp
--     , VG.Vector childC (QDiagram SVG R2 Any)
--     , Integral (Scalar (leafC dp))
--     , Integral (Scalar (childC (CoverTree_ exprat childC leafC dp)))
--     ) => P.FilePath
--       -> CoverTree_ exprat childC leafC dp
--       -> IO ()
drawCT path ct = renderSVG path (Dims 500 300) (diagramCT_ 0 ct)


-- diagramCT node = diagramCT_ 0 node

-- type instance Scalar R2 = Double

-- diagramCT_ ::
--     ( ValidCT exprat childC leafC dp
--     ) => Int
--       -> CoverTree_ exprat childC leafC dp
--       -> Diagram a R2
diagramCT_ (depth::Int) tree
    = mkConnections $
        ( named (label++show depth) $ fontSize (Global 0.01) $
            (
                (text label D.<> strutY 0.5)
            === (text (show (sepdist tree)) D.<> strutY 0.5)
            -- === (text (show (maxDescendentDistance tree)) <> strutY 0.5)
            )
        D.<> circle 1 # fc nodecolor
        )
    === (pad 1.05 $ centerName (label++show (depth+1)) $
        VG.foldr (|||) mempty $ VG.map (diagramCT_ (depth+1)) $ children tree)

    where
        label = intShow $ nodedp tree
        nodecolor = if ctBetterMovableNodes tree==0 --nodeWeight tree > 0
            then red
            else lightblue

        mkConnections =
            D.connect (label++show depth) (label++show (depth+1))
            . apList (fmap
                (\key -> D.connect (label++show depth) (intShow key++show (depth+1)))
                (map nodedp $ toList $ children tree)
                )

centerName name = withName name $ \b a -> moveOriginTo (location b) a

apList :: [a -> a] -> a -> a
apList [] a = a
apList (x:xs) a = apList xs (x a)

intShow :: Show a => a -> String
intShow a = P.filter go $ show a
    where
        go x
            | x=='.' = True
            | x==',' = True
            | x=='-' = True
            | x=='1' = True
            | x=='2' = True
            | x=='3' = True
            | x=='4' = True
            | x=='5' = True
            | x=='6' = True
            | x=='7' = True
            | x=='8' = True
            | x=='9' = True
            | x=='0' = True
            | otherwise = False

