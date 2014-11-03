{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.CoverTree
    where

import Debug.Trace
import qualified Data.List as L
import qualified Prelude as P

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Control.DeepSeq

import Data.Params

import SubHask
import SubHask.Algebra.Vector
import HLearn.DataStructures.SpaceTree
-- import HLearn.Metrics.Lebesgue
import HLearn.Models.Distributions.Univariate.Normal

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

deriving instance
    ( Eq (childC (CoverTree_ exprat childC leafC dp))
    , Eq (leafC dp)
    , Eq dp
    , Eq (Scalar dp)
    ) => Eq (CoverTree_ exprat childC leafC dp)

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

{-# INLINE exprat #-}
-- exprat :: Field (Scalar dp) => CoverTree_ exprat childC leafC dp -> Scalar dp
exprat :: Field r => r
exprat = 1.3

type ValidCT exprat childC leafC dp =
    ( Foldable (childC (CoverTree_ exprat childC leafC dp))
    , Foldable (leafC dp)
    , Elem (childC (CoverTree_ exprat childC leafC dp)) ~ CoverTree_ exprat childC leafC dp
    , Elem (leafC dp) ~ dp
    , VG.Vector childC (CoverTree_ exprat childC leafC dp)
    , VG.Vector childC (Scalar dp)
    , VG.Vector leafC dp
    , VG.Vector leafC (Scalar dp)
    , MetricSpace dp
    , QuotientField (Scalar dp) Int
    , Floating (Scalar dp)
    , MinBound (Scalar dp)
    , MaxBound (Scalar dp)
    , Ord dp
    , KnownFrac exprat
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

    {-# INLINE stIsMinDistanceDpFartherThanWithDistance #-}
    {-# INLINE stIsMaxDistanceDpFartherThanWithDistance #-}

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
        $ (maxDescendentDistance ct / coverdist ct)
        : map ctMaxCoverRatio (toList $ children ct)
    else 0

-- | returns a measure of how "spread out" descendent points are
{-# INLINABLE ctAveCoverRatio #-}
ctAveCoverRatio ::
    ( ValidCT exprat childC leafC dp
    , Scalar (childC (CoverTree_ exprat childC leafC dp)) ~ Scalar (leafC dp)
    ) => CoverTree_ exprat childC leafC dp -> Normal (Scalar dp)
ctAveCoverRatio ct = if length (children ct) + length (leaves ct) > 0
    then train1Normal (maxDescendentDistance ct / coverdist ct)
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
            $ map (\p -> distance (nodedp c) (nodedp p)
                       < distance (nodedp c) (nodedp realparent))
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
invariant_CoverTree_covering node = if not $ stHasNoChildren node
    then (maximum.toList) (VG.map (distance (nodedp node) . nodedp) $ children node) < coverdist node
      && and (VG.map invariant_CoverTree_covering $ children node)
    else True

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
      >= sepdist node
   && and (VG.map invariant_CoverTree_separating $ children node)
    where
        cartesianProduct :: [a] -> [b] -> [(a,b)]
        cartesianProduct xs ys = P.concatMap (\y -> map (\x -> (x,y)) xs) ys

invariant_maxDescendentDistance ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_maxDescendentDistance node
    = and (map invariant_maxDescendentDistance $ stChildrenList node)
   && and (map (\dp -> distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

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
--     , maxDescendentDistance = max
--         ( maximum $ 0 : ( VG.toList $ VG.map (\c -> distance (nodedp ct) (nodedp c) + maxDescendentDistance c) children' ) )
--         ( maximum $ 0 : ( VG.toList $ VG.map (distance $ nodedp ct) $ leaves ct ) )
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

instance Semigroup Ordering where
    LT+_=LT
    GT+_=GT
    EQ+x=x

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

{-# INLINABLE packCT #-}
packCT ::
    ( ValidCT exprat childC leafC dp
    , VG.Vector leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
-- packCT ct = {-# SCC packCT #-} sndHask $ go 0 ct'
packCT !ct = {-# SCC packCT #-} P.snd $ go 0 ct'
    where
        go !i !t = {-# SCC packCT_go #-} ( i',t
            { nodedp = v VG.! i
            , leaves = VG.slice (i+1) (VG.length $ leaves t) v
            , children = fromList children'
            } )
            where
                (i',children') = {-# SCC mapAccumL #-} L.mapAccumL
                    go
                    (i+1+length (toList $ leaves t))
                    (toList $ children t)

        ct' = setLeaves 0 ct
        v = {-# SCC fromList #-} fromList $ mkNodeList ct'

        mkNodeList ct = {-# SCC mkNodeList #-} [nodedp ct]
                     ++ (toList $ leaves ct)
                     ++ (L.concatMap mkNodeList $ toList $ children ct)

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

-------------------------------------------------------------------------------
-- construction

{-# INLINABLE trainInsert #-}
trainInsert ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsert xs = case unCons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ foldr' insertCT (train1dp dp) dps

{-# INLINABLE trainMonoid #-}
trainMonoid ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainMonoid xs = trace "trainMonoid" $ foldMap (Just' . train1dp) xs

{-# INLINABLE train1dp #-}
train1dp ::
    ( ValidCT exprat childC leafC dp
    ) => dp -> CoverTree_ exprat childC leafC dp
train1dp dp = Node
    { nodedp                = dp
    , nodeWeight            = 1
    , level                 = minBound
    , numdp                 = 1
    , children              = empty
    , leaves                = empty
    , maxDescendentDistance = 0
    }

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

{-# INLINABLE insertCT #-}
insertCT :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCT dp ct = {-# SCC insertCT #-} if dist > coverdist ct
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

            -- | FIXME: can we implement this using a scan?
            go acc []     = {-# SCC go #-} ((singletonCT dp) { level = level ct-1 }):acc
            go acc (x:xs) = {-# SCC go #-} if isFartherThan (nodedp x) dp (sepdist ct)
                then go (x:acc) xs
                else acc+((insertCT dp x):xs)

instance
    ( ValidCT exprat childC leafC dp
    ) => Abelian (CoverTree_ exprat childC leafC dp)

instance
    ( ValidCT exprat childC leafC dp
    ) => Semigroup (CoverTree_ exprat childC leafC dp)
        where
    {-# INLINABLE (+) #-}
    ct1 + ct2 = {-# SCC semigroup #-} case ctmerge ct1 ct2 of
        (ct, []) -> ct
        (ct, xs) -> foldl' (+) ct xs

ctmerge :: forall exprat childC leafC  dp.
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
      -> ( CoverTree_ exprat childC leafC dp
         , [CoverTree_ exprat childC leafC dp]
         )
ctmerge ct1 ct2 =
    if isFartherThan (nodedp ct1) (nodedp ct2) (coverdist ct1)
        then ctmerge' ct1 ct2
        else ctmerge' ct1' ct2'
    where
        ct1' = raiseRootLevel maxlevel ct1
        ct2' = raiseRootLevel maxlevel ct2
        maxlevel = maximum
            [ level ct1
            , level ct2
            , dist2level_down (Proxy::Proxy exprat) $ distance (nodedp ct1) (nodedp ct2)
            ]

ctmerge' :: forall exprat childC leafC  dp.
    ( ValidCT exprat childC leafC  dp
    ) => CoverTree_ exprat childC leafC  dp
      -> CoverTree_ exprat childC leafC  dp
      -> -- Maybe'
            ( CoverTree_ exprat childC leafC  dp
            , [CoverTree_ exprat childC leafC  dp]
            )
ctmerge' ct1 ct2 = {-# SCC ctmerge' #-}
    ( insertCT (nodedp ct2) $ ct1
        { children = children'
        , numdp = sum $ map numdp $ toList children'
        , maxDescendentDistance = coverdist ct1
        }
    , invalidchildren++invalid_newleftovers
    )
    where
        children' = fromList (elems newchildren + valid_newleftovers)

        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (coverdist ct1)
        (validchildren,invalidchildren) = L.partition validchild $ stChildrenList ct2

        -- | FIXME: replace go with fold, better variable names
        -- FIXME: if CT is an IndexedVector, we can get rid of the map
        (newchildren,newleftovers) = go ({--}ct2indexedVector ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = L.partition validchild newleftovers

        go (childmap,leftovers) []     = (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = {-# SCC ctmerge'_go #-}
            go ( insertAt (nodedp new) (new { level = level ct1-1 }) childmap
               , leftovers
               ) xs
            where
                (new,leftovers) = case L.filter (\v -> isFartherThan (nodedp v) (nodedp x) (coverdist v)) $ elems childmap of
                    [] -> (x,leftovers)
                    y:_ -> let (new,ys) = ctmerge' y x in (new,ys+leftovers)

{-
instance
    ( ValidCT exprat childC leafC dp
    ) => Semigroup (CoverTree_ exprat childC leafC dp)
        where
    {-# INLINABLE (+) #-}
    ct1 + ct2 = {-# SCC semigroup #-} case ctmerge' ct1' ct2' of
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

ctmerge' :: forall exprat childC leafC  dp.
    ( ValidCT exprat childC leafC  dp
    ) => CoverTree_ exprat childC leafC  dp
      -> CoverTree_ exprat childC leafC  dp
      -> Maybe'
            ( CoverTree_ exprat childC leafC  dp
            , [CoverTree_ exprat childC leafC  dp]
            )
ctmerge' ct1 ct2 = {-# SCC ctmerge' #-}
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

        (newchildren,newleftovers) = go (ct2indexedVector ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = L.partition validchild newleftovers

        go (childmap,leftovers) []     = (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = {-# SCC ctmerge'_go #-}
            case
                L.filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge'' v x)) $ {-Map.assocs-}toIndexedList childmap of
--                 L.filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge'' v x)) $ {-Map.assocs-}toIndexedList childmap of
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
                ctmerge'' ct1 ct2 = {-# SCC ctmerge'' #-} ctmerge' ct1 ct2
                    where
                        ct1' = raiseRootLevel maxlevel ct1
                        ct2' = raiseRootLevel maxlevel ct2
                        maxlevel = maximum
                            [ level ct1
                            , level ct2
                            , dist2level_down (Proxy::Proxy exprat) $ distance (nodedp ct1) (nodedp ct2)
                            ]

-}

ct2indexedVector ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> IndexedVector dp (CoverTree_ exprat childC leafC dp)
ct2indexedVector ct = fromIndexedList $ map (\v -> (nodedp v,v)) $ stChildrenList ct

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

-- v1 = [1,2,3] :: (Array VU.Vector) Float
-- v1 = VG.fromList [1,2,3] :: L2 VU.Vector Float
-- v2 = VG.fromList [1,2,2] :: L2 VU.Vector Float
-- v3 = VG.fromList [2,2,2] :: L2 VU.Vector Float
-- v4 = VG.fromList [3,1,2] :: L2 VU.Vector Float
