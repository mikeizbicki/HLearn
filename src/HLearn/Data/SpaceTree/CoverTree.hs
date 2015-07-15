-- | This module provides an interface to the cover tree data structure.
-- Cover trees are used for fast nearest neighbor queries.
-- They are similar to kd-trees, but they are faster in high dimensions and can be used on more types of data.
-- In particular, they can be used on anything that is an instance of "Metric".
--
-- See the paper <http://izbicki.me/public/papers/icml2015-faster-cover-trees.pdf Faster Cover Trees> for a detailed explanation of the theory.
-- I'm happy to answer any questions you have about the implementation details here.
module HLearn.Data.SpaceTree.CoverTree
    ( BCoverTree
    , UCoverTree
    , CoverTree_
    , ValidCT

    -- * Debug interface
    -- | The functions below are not intended for standard use.
    -- They provide an advanced interface to assist with development.
    -- Instead, you should use the functions from the "Container" hierarchy of classes provided by subhask.

    -- ** Optimizations
    , ctAdoptNodes
    , setMaxDescendentDistance
    , sortChildren
    , cmp_numdp_distance
    , cmp_numdp_distance'
    , cmp_distance_numdp
    , cmp_distance_numdp'
    , setLeaves
    , packCT

    -- ** Alternative Construction Methods
    , trainMonoid

    , trainInsertOrig
    , trainInsertNoSort
    , trainInsert

    , insertCTOrig
    , insertCTNoSort
    , insertCT

    , AddChildMethod
    , addChild_nothing
    , addChild_parent
    , addChild_ancestor

    -- ** Performance Measures
    , ctAveCoverRatio
    , ctMaxCoverRatio
    , ctMovableParents
    , ctBetterMovableParents
    , ctMovableNodes
    , ctBetterMovableNodes

    -- ** Invariants
    , invariant_CoverTree_covering
    , invariant_CoverTree_tightCovering
    , invariant_CoverTree_maxDescendentDistance
    , invariant_CoverTree_separating
    )
    where

import qualified Data.List as L
import qualified Prelude as P

import SubHask
import SubHask.Monad
import SubHask.Algebra.Array
import SubHask.Algebra.Container
import SubHask.Algebra.Ord
import SubHask.Algebra.Vector
import SubHask.Compatibility.Containers

import HLearn.Data.SpaceTree
import HLearn.Data.SpaceTree.CoverTree.Unsafe
import HLearn.Models.Distributions

import Debug.Trace

-------------------------------------------------------------------------------

-- | The type of boxed cover trees.
type BCoverTree = CoverTree_ () BArray BArray

-- | The type of unboxed cover trees.
-- Typically more than 2x faster than boxed cover trees.
type UCoverTree = CoverTree_ () BArray UArray

-- | A generic type of cover tree that let's the user specify a number of internal parameters about how memory is represented internally.
-- You probably don't want to use this type.
data CoverTree_
        ( exprat                :: * ) -- FIXME: should be "Frac" from typeparams
        ( childC                :: * -> * )
        ( leafC                 :: * -> * )
        ( dp                    :: * )
    = Node
        { nodedp                :: !dp
        , level                 :: {-#UNPACK#-}!Int
        , nodeWeight            :: !(Scalar dp)
        , numdp                 :: !(Scalar dp)
        , maxDescendentDistance :: !(Scalar dp)
        , children              :: !(childC (CoverTree_ exprat childC leafC dp))
        , leaves                :: !(leafC dp)
--         { nodedp                :: {-#UNPACK#-}!(Labeled' (UVector "dyn" Float) Int)
--         , nodeWeight            :: {-#UNPACK#-}!Float
--         , level                 :: {-#UNPACK#-}!Int
--         , numdp                 :: {-#UNPACK#-}!Float
--         , maxDescendentDistance :: {-#UNPACK#-}!Float
--         , children              :: {-#UNPACK#-}!(BArray (CoverTree_ exprat childC leafC dp))
--         , leaves                :: {-#UNPACK#-}!(UArray dp)
        }

mkMutable [t| forall a b c d. CoverTree_ a b c d |]

type instance Scalar (CoverTree_ exprat childC leafC dp) = Scalar dp
type instance Logic (CoverTree_ exprat childC leafC dp) = Bool
type instance Elem (CoverTree_ exprat childC leafC dp) = dp
type instance SetElem (CoverTree_ exprat childC leafC dp) dp' = CoverTree_ exprat childC leafC dp'

-- | This type alias simplifies all our type signatures.
-- It ensures that a "CoverTree_"'s parameters are sufficient to carry out the necessary operations.
--
-- FIXME:
-- Trim this set of constraints to be more manageable.
--
-- FIXME:
-- There is a much smaller subset of these that is actually needed,
-- but GHC's type system isn't strong enough to express this subset.
type ValidCT exprat childC leafC dp =
    ( Foldable (childC (CoverTree_ exprat childC leafC dp))
    , Foldable (leafC dp)
    , Constructible (childC (CoverTree_ exprat childC leafC dp))
    , Constructible (leafC dp)
    , IxContainer (childC (CoverTree_ exprat childC leafC dp))
    , Sliceable (leafC dp)
    , Normed (childC (CoverTree_ exprat childC leafC dp))
    , Normed (leafC dp)
    , Elem (childC (CoverTree_ exprat childC leafC dp)) ~ CoverTree_ exprat childC leafC dp
    , Elem (leafC dp) ~ dp
    , Metric dp
    , QuotientField (Scalar dp) Int
    , Real (Scalar dp)
    , Bounded (Scalar dp)
--     , CanError (Scalar dp)
    , Logic dp ~ Bool
    , Logic (CoverTree_ exprat childC leafC dp) ~ Bool
    , Logic (Scalar (leafC dp)) ~ Bool
    , NFData (CoverTree_ exprat childC leafC dp)
    , HasScalar dp
    , Eq_ (childC (CoverTree_ exprat childC leafC dp))
    , Index (leafC dp) ~ Int
    , Scalar (childC (CoverTree_ exprat childC leafC dp)) ~ Int

    , ClassicalLogic (leafC dp)
    , ClassicalLogic (leafC (CoverTree_ exprat childC leafC dp))
    , ClassicalLogic (childC (CoverTree_ exprat childC leafC dp))
    , Container (leafC dp)
    , Container (childC (CoverTree_ exprat childC leafC dp))

    -- ghc7.10
    , Elem (childC (CoverTree_ exprat childC leafC dp)) ~ (CoverTree_ exprat childC leafC dp)
    , Scalar (childC (CoverTree_ exprat childC leafC dp)) ~ Scalar (leafC dp)
    , Hilbert (Scalar (Elem (childC (Scalar dp))))
    , FiniteModule (Scalar (Elem (childC (Scalar dp))))

    -- unpack
--     , dp ~ Labeled' (UVector "dyn" Float) Int
--     , childC~BArray
--     , leafC~UArray

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

-------------------------------------------------------------------------------
-- type classes

instance
    ( ValidCT exprat childC leafC dp
    ) => SpaceTree (CoverTree_ exprat childC leafC) dp
        where

    type ChildContainer (CoverTree_ exprat childC leafC ) = childC
    type LeafContainer (CoverTree_ exprat childC leafC ) = leafC

    stChildren  = children
    stLeaves    = leaves
    stNode      = nodedp
    stWeight    = nodeWeight

    stMaxDescendentDistance = maxDescendentDistance

----------------------------------------
-- misc type classes

deriving instance
    ( Show (childC (CoverTree_ exprat childC leafC dp))
    , Show (leafC dp)
    , Show dp
    , Show (Scalar dp)
    , ValidCT exprat childC leafC dp
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

instance
    ( Arbitrary dp
    , ValidCT exprat childC leafC dp
    ) => Arbitrary (CoverTree_ exprat childC leafC dp) where
    arbitrary = do
        x  <- arbitrary
        xs <- arbitrary
        return $ fromList1 x xs

----------------------------------------
-- comparison

instance ValidCT exprat childC leafC dp => Eq_ (CoverTree_ exprat childC leafC dp) where
    ct1==ct2 = stToList ct1==stToList ct2

-- FIXME: need proper MultiSet type to implement
--
-- instance
--     ( ValidCT exprat childC leafC dp
--     ) => POrd_ (Maybe' (CoverTree_ exprat childC leafC dp))
--         where
--
--     inf ct1 ct2 = trainInsert addChild_ancestor
--         $ inf (convertContainer ct1) (convertContainer ct2 :: Set dp)
--         where
--             convertContainer = fromList . stToList
--
-- instance
--     ( ValidCT exprat childC leafC dp
--     ) => Lattice_ (Maybe' (CoverTree_ exprat childC leafC dp))
--         where
--
--     sup ct1 ct2 = trainInsert addChild_ancestor
--         $ sup (convertContainer ct1) (convertContainer ct2 :: Set dp)
--         where
--             convertContainer = fromList . stToList

----------------------------------------
-- algebra

instance ValidCT exprat childC leafC dp => Normed (CoverTree_ exprat childC leafC dp) where
    size = numdp

----------------------------------------
-- container

instance
    ( ValidCT exprat childC leafC dp
    ) => Constructible (CoverTree_ exprat childC leafC dp)
        where

    singleton = singletonCT
    fromList1 x xs = fromJust' $ {-packCT $ setLeaves 0 $-} trainInsert addChild_ancestor (x:xs)
    -- FIXME:
    -- We can't pack the cover tree because insertion and (+) currently ignore datapoints in the leafContainer

instance
    ( ValidCT exprat childC leafC dp
    ) => Container (CoverTree_ exprat childC leafC dp)
        where

    -- FIXME: use the covertree's structure!
    elem e ct = elem e $ stToList ct

-- FIXME:
-- We need to make subhask's hierarchy a bit more generic before we can make cover trees foldable.
--
-- instance
--     ( ValidCT exprat childC leafC dp
--     ) => Foldable (CoverTree_ exprat childC leafC dp)
--         where
--     toList = stToList

-------------------------------------------------------------------------------
-- construction

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

----------------------------------------

-- | Construct a cover tree using the original algorithm.
{-# INLINABLE trainInsertOrig #-}
trainInsertOrig ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsertOrig xs = {-# SCC trainInsertOrig #-}case uncons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ rmSingletons $ foldr' insertCTOrig (singletonCT dp) dps

    where
        -- Removes nodes that have only a single child that contains the same datapoint.
        -- These singleton nodes are not supposed to be included in the original cover tree's expicit representation,
        -- but the way our insertion method works it is easiest to add them then remove them at the end of insertion.
        -- The new insertion methods can't create singleton nodes.
        rmSingletons ::
            ( ValidCT exprat childC leafC dp
            ) => CoverTree_ exprat childC leafC dp
              -> CoverTree_ exprat childC leafC dp
        rmSingletons ct =  if isSingleton
            then  rmSingletons onlyChild
            else ct
                { children = fromList $ map rmSingletons $ toList $ children ct
                }
            where
                isSingleton = size (children ct) == 1
                           && nodedp onlyChild == nodedp ct

                onlyChild = P.head $ toList $ children ct

-- | Insert a node into a cover tree using the original algorithm.
{-# INLINABLE insertCTOrig #-}
insertCTOrig ::
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCTOrig dp ct = insertCTOrig_ dp ct (distance dp (nodedp ct))

-- | Helper function required for GHC's optimizer to work properly.
{-# INLINABLE insertCTOrig_ #-}
insertCTOrig_ :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> Scalar dp
      -> CoverTree_ exprat childC leafC dp
insertCTOrig_ !dp !ct !dist = {-# SCC insertCTOrig_ #-}
    if dist > coverdist ct

        -- ct can't cover dp, so create a new node at dp that covers ct
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

        -- insert dp underneath ct
        else ct
            { numdp                 = numdp ct+1
            , maxDescendentDistance = max dist (maxDescendentDistance ct)
            , children              = fromList $ go [] $ toList $ children ct
            }

        where
            go !acc (x:xs) = if isFartherThan (nodedp x) dp (sepdist ct)
                then go (x:acc) xs
                else acc+((insertCTOrig dp x):xs)

            go !acc [] = if dist >= sepdist ct

                -- far from root, so just insert the node
                then ((singletonCT dp) { level = level ct-1 }):acc

                -- close to root, so add new lower root level
                else insertCTOrig_ dp ct' dist:acc
                    where
                        ct' = (singletonCT (nodedp ct))
                            { level      = level ct-1
                            , numdp      = 0
                            , nodeWeight = 0
                            }

----------------------------------------

-- | This corresponds to the simplified cover tree in the ICML paper.
{-# INLINABLE trainInsertNoSort #-}
trainInsertNoSort ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsertNoSort xs = {-# SCC trainInsertNoSort #-}case uncons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ foldr' insertCTNoSort (singletonCT dp) dps

-- | This corresponds to the simplified cover tree in the ICML paper.
{-# INLINABLE insertCTNoSort #-}
insertCTNoSort :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCTNoSort dp ct = {-# SCC insertCTNoSort #-} insertCTNoSort_ dp ct $ distance dp (nodedp ct)

-- | Helper function required for GHC's optimizer to work properly.
{-# INLINABLE insertCTNoSort_ #-}
insertCTNoSort_ :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => dp
      -> CoverTree_ exprat childC leafC dp
      -> Scalar dp
      -> CoverTree_ exprat childC leafC dp
insertCTNoSort_ dp ct dist = {-# SCC insertCTNoSort_ #-}
    if dist > coverdist ct

        -- ct can't cover dp, so create a new node at dp that covers ct
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

        -- insert dp underneath ct
        else ct
            { numdp                 = numdp ct+1
            , maxDescendentDistance = max dist (maxDescendentDistance ct)
            , children              = {-# SCC conv #-} fromList $ go [] $ toList $ children ct
            }

        where
            go !acc []     = {-# SCC go #-} ((singletonCT dp) { level = level ct-1 }):acc
            go !acc (x:xs) = {-# SCC go #-} if isFartherThan (nodedp x) dp (sepdist ct)
                then go (x:acc) xs
                else acc+((insertCTNoSort dp x):xs)

----------------------------------------

-- | Provides a generic insertion method that lets us enforce a whole suite of invariants related to the nearest ancestor invariant of the ICML paper.
{-# INLINABLE trainInsert #-}
trainInsert ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => AddChildMethod exprat childC leafC (Elem xs)
      -> xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainInsert addChild xs = {-# SCC trainInsert #-} case uncons xs of
    Nothing -> Nothing'
    Just (dp,dps) -> Just' $ foldr' (insertCT addChild) (singletonCT dp) $ toList dps

-- | Insert a single data point into the cover tree.
{-# INLINABLE insertCT #-}
insertCT :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCT addChild dp ct =
    insertCT_ addChild dp ct (distance dp $ nodedp ct)

-- | This function is exactly the same as insertCT.
-- We need to provide a different function, however, for performance reasons.
-- There's many weird layers of recursion going on in these functions,
-- and this was preventing GHC from inlining/specializing these functions.
-- Now, insertCT is not recussive, so GHC can trivially inline it.
{-# INLINABLE insertCT_internal #-}
insertCT_internal :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
insertCT_internal addChild dp ct =
    insertCT_ addChild dp ct (distance dp $ nodedp ct)

-- | Like "insertCT", but this function also takes the distance between the data point and root of the cover tree to avoid recomputation.
{-# INLINABLE insertCT_ #-}
insertCT_ :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> Scalar dp
      -> CoverTree_ exprat childC leafC dp
insertCT_ addChild dp ct dist = {-# SCC insertCT_ #-}
    if dist > coverdist ct
        -- ct can't cover dp, so create a new node at dp that covers ct
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

        -- insert dp underneath ct
        else ct
            { numdp                 = numdp ct+1
            , maxDescendentDistance = max dist (maxDescendentDistance ct)
            , children              = fromList $ go [] childrendists
            }

        where
            childrendists = map (\x -> (distance dp (nodedp x), x)) $ toList $ children ct

            mindist = minimum $ map fst childrendists

            go !acc [] = addChild dp ct
            go !acc ((xdist,x):xs) = if xdist == mindist && xdist <= coverdist x
                then insertCT_ addChild dp x xdist:(acc+map snd xs)
                else go (x:acc) xs

{-
 - FIXME:
 - The code below enforces a "repulsion" invariant.
 - This speeds up nearest neighbor queries in practice.
 - But this implementation breaks the ancestor invariant.
 - It shouldn't be too hard to fix,
 - but I'm pretty sick of thinking about cover trees right now and don't want to do it.

            childrendists = map (\x -> (distance dp (nodedp x), x)) $ toList $ children ct

            (mindist:mindist2:_) = (sort $ map fst childrendists)++[maxBound,maxBound]

            go !acc [] = addChild dp ct
            go !acc ((xdist,x):xs) = if xdist == mindist && xdist <= coverdist x
                then insertswapper x xdist:(acc+map snd xs)
                else go (x:acc) xs

            insertswapper x xdist = if mindist2 > sepdist ct
--                                     && distance (nodedp ct) dp > distance (nodedp ct) (nodedp x)
--                                     && distance (nodedp ct) dp > sepdist ct
                                    && maximum dpdists > maximum xdists
                                    && maximum xchilddists < coverdist x
                                    && and (map (\t -> size (stChildrenList t)==0) $ stChildrenList x)
                then insertCT_ addChild (nodedp x) (x {nodedp=dp}) xdist
                else insertCT_ addChild dp x xdist
                    where
                        dpdists = map (\x -> distance dp (nodedp x)) $ toList $ children ct
                        xdists = map (\x' -> distance (nodedp x) (nodedp x')) $ toList $ children ct
                        xchilddists = map (distance dp . nodedp) (stChildrenList x)
-}

-- | These functions control the invariants we maintain while performing insertions into the cover tree.
-- The input data point will be directly inserted as one of the children of the root of the tree.
-- Therefore, we require it satisfy the separating and covering conditions (and any other applicable conditions).
-- The function may (possibly) move points from other nodes into this new child to enforce other invariants.
-- The return value is the list of new children for the root node after the insertion has taken place.
--
-- NOTE:
--
-- We could make this type synonym have a much "prettier" interface like so:
--
-- > type AddChildMethod dp = forall exprat childC leafC.
-- >     ValidCT exprat childC leafC dp
-- >         => dp
-- >         -> CoverTree_ exprat childC leafC dp
-- >         -> [CoverTree_ exprat childC leafC dp]
--
-- But this has devastating effects on performance.
-- It combines with the weird recursive calls to prevent inlining, which results in >10x slow downs.
type AddChildMethod exprat childC leafC dp
        =  dp
        -> CoverTree_ exprat childC leafC dp
        -> [CoverTree_ exprat childC leafC dp]

{-# INLINABLE addChild_nothing #-}
addChild_nothing ::
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
addChild_nothing dp ct = cons
    ( (singletonCT dp) { level = level ct-1} )
    (toList $ children ct)

{-# INLINABLE addChild_parent #-}
addChild_parent ::
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
addChild_parent dp ct = {-# SCC addChild_parent #-} ret:acc'
    where
        (acc',dps) = rmCloseChildren addChild_parent dp  $ toList $ children ct
        ct'=(singletonCT dp) { level = level ct-1 }
        ret=foldr' (insertCT_internal addChild_parent) ct' $ concat $ map stToList dps

{-# INLINABLE addChild_ancestor #-}
addChild_ancestor :: forall exprat childC leafC dp k.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
addChild_ancestor dp ct = {-# SCC addChild_ancestor #-}
    toList $ children $ foldr' (insertCT addChild_ancestor) ct' (concat $ map stToList dps)
    where
        ( acc', dps ) = rmCloseChildren addChild_ancestor dp  $ toList $ children ct

        ct' :: CoverTree_ exprat childC leafC dp
        ct' = ct
            { children = fromList $ ((singletonCT dp) { level = level ct-1 }) : acc'
            }

{-# INLINABLE rmCloseChildren #-}
rmCloseChildren ::
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
      -> dp
      -> [CoverTree_ exprat childC leafC dp]
      -> ( [CoverTree_ exprat childC leafC dp]
         , [CoverTree_ exprat childC leafC dp]
         )
rmCloseChildren addChild dp cts = {-# SCC rmCloseChildren #-}
    (map fst xs, P.concat $ map snd xs)
    where
        xs = map (extractCloseChildren addChild dp) cts

{-# INLINABLE extractCloseChildren #-}
extractCloseChildren :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => AddChildMethod exprat childC leafC dp
      -> dp
      -> CoverTree_ exprat childC leafC dp
      -> ( CoverTree_ exprat childC leafC dp
         , [CoverTree_ exprat childC leafC dp]
         )
extractCloseChildren addChild dp root = {-# SCC extractCloseChildren #-}
    case go root of
        (Just' x,xs) -> (x,xs)
    where
        go ct = if dist_ct_root + maxDescendentDistance ct < dist_ct_dp
            then (Just' ct,[])
            else if dist_ct_root > dist_ct_dp
                then (Nothing', [ct])
                else
                    ( Just' $ ct
                        { children = fromList children'
                        , numdp    = nodeWeight ct + sum (map numdp children')
                        }
                    , concat $ map snd allbabies
                    )
                where
                    allbabies = map go $ toList $ children ct
                    children' = justs' $ map fst allbabies

                    dist_ct_root = distance (nodedp ct) (nodedp root)
                    dist_ct_dp   = distance (nodedp ct) dp

{-# INLINABLE moveLeafToRoot #-}
moveLeafToRoot :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> Maybe' (CoverTree_ exprat childC leafC dp)
moveLeafToRoot ct = if stHasNoChildren ct
    then Nothing'
    else Just' $ ct
        { nodedp   = nodedp leaf
        , numdp    = numdp ct - numdp leaf
        , children = children ct'
        , maxDescendentDistance = coverdist ct*2
        }
    where
        (leaf,ct') = rmleaf ct

--------------------------------------------------------------------------------

{-# INLINABLE trainMonoid #-}
trainMonoid ::
    ( ValidCT exprat childC leafC (Elem xs)
    , Foldable xs
    ) => xs
      -> Maybe' (CoverTree_ exprat childC leafC (Elem xs))
trainMonoid xs = {-# SCC trainMonoid #-} foldtree1 $ map (Just' . singletonCT) $ toList xs

instance
    ( ValidCT exprat childC leafC dp
    ) => Semigroup (CoverTree_ exprat childC leafC dp)
        where

    {-# INLINABLE (+) #-}
    ct1 + ct2 = {-# SCC semigroup_CoverTree #-} case ctmerge_ ct1_ ct2_ dist of
        (ct, []) -> ct
        (ct, xs) -> {-# SCC sg_foldr #-} foldr' insertCTNoSort ct $ concat $ map stToList xs

        where
            dist = distance (nodedp ct1) (nodedp ct2)

            maxlevel = maximum
                [ level ct1
                , level ct2
                , dist2level_down (Proxy::Proxy exprat) dist
                ]

            ct1_ = if level ct1 < maxlevel then raiseRootLevel maxlevel ct1 else ct1
            ct2_ = if level ct2 < maxlevel then raiseRootLevel maxlevel ct2 else ct2

-- | If the following prerequisites are met:
--
-- > level ct1==level ct2
--
-- > distance (nodedp ct1) (nodedp ct2) < coverdist ct1
--
-- then all output covertrees will be valid.
-- The root of ct1 is guaranteed to not change.
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
            = {-# SCC sg_foldl #-} foldl' (go []) (stChildrenList ct1,[],[]) coveredChildren
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
        ct_minusleftovers = {-# SCC minusleftover #-} insertCTNoSort_
            (nodedp ct2)
            ( ct1
                { children = fromList $ children' + sepcovChildren
                , numdp =  sum $ map numdp $ children' + sepcovChildren
                , maxDescendentDistance = coverdist ct1*2
                } )
            dist

        -- insert any leftovers into the tree if they fit
        (ct',leftovers') = {-# SCC part4 #-} foldl' go (ct_minusleftovers,[]) leftovers
            where
                go (ct,xs) x = if level ct >= level x && godist <= coverdist ct
                    then (goct, goleftovers++xs)
                    else (ct,x:xs)
                    where
                        godist = distance (nodedp ct) (nodedp x)
                        (goct,goleftovers) = ctmerge_ ct (raiseRootLevel (level ct) x) godist

-- | FIXME: does this work if there are leaves present?
{-# INLINABLE raiseRootLevel #-}
raiseRootLevel :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => Int
      -> CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
raiseRootLevel i ct
    | stHasNoChildren ct = {-# SCC raiseRootLevel #-}ct { level = i }
    | i <  level ct = error
        $ "\nraiseRootLevel: target level less than current tree level"
        + "; i="+show i
        + "; level ct="+show (level ct)
        + "; nodedp ct="+show (nodedp ct)
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
rmleaf ct = {-# SCC rmleaf #-} if stHasNoChildren head
    then ( head
         , ct
            { numdp    = numdp ct-nodeWeight head
            , children = tail
            }
         )
    else ( itrleaf
         , ct
            { numdp    = numdp ct-nodeWeight head
            , children = itrtree `cons` tail
            }
         )
    where
        (itrleaf,itrtree) = rmleaf head

        childL = children ct
        (head,tail)  = case uncons childL of Just x -> x

--------------------------------------------------------------------------------
-- Performance measurements

-- | returns a measure of how "spread out" descendent points are
{-# INLINABLE ctMaxCoverRatio #-}
ctMaxCoverRatio ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp -> Scalar dp
ctMaxCoverRatio ct = if size (children ct) + size (leaves ct) > 0
    then maximum
        $ (stMaxDescendentDistance ct / coverdist ct)
        : map ctMaxCoverRatio (toList $ children ct)
    else 0

-- | returns a measure of how "spread out" descendent points are
{-# INLINABLE ctAveCoverRatio #-}
ctAveCoverRatio ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp -> Normal (Scalar dp)
ctAveCoverRatio ct = if size (children ct) + size (leaves ct) > 0
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
            - (fromIntegral $ toInteger $ size $ children c)
            - (fromIntegral $ toInteger $ size $ leaves c)

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
            - (fromIntegral $ toInteger $ size $ children c)
            - (fromIntegral $ toInteger $ size $ leaves c)

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

        totalNodesOfChildren c
            = indicator
            $ (> (1::Int))
            $ sum
            $ map (\p -> indicator $ distance (nodedp c) (nodedp p) < sepdist ct)
            $ toList (children ct)
           ++ toList (map singletonCT $ toList $ leaves ct)

-- | Counts the number of nodes in the tree with uncles they would be better placed under
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

        totalBetterNodesOfChildren realparent c
            = indicator
            $ or
            $ map (\p -> distance (nodedp c) (nodedp p)
                       < distance (nodedp c) (nodedp realparent)
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
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_covering node
    = and (map invariant_CoverTree_covering $ stChildrenList node)
   && and (map (\child -> distance  (nodedp node) (nodedp child) <= coverdist node) $ stChildrenList node)

invariant_CoverTree_tightCovering ::
    ( ValidCT exprat childC leafC dp
    , Elem (childC (Scalar dp)) ~ Scalar dp
    , Elem (childC Bool) ~ Bool
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_tightCovering node
    = and (map invariant_CoverTree_tightCovering $ stChildrenList node)
   && and (map (\dp -> distance dp (nodedp node) <= coverdist node) $ stDescendents node)

invariant_CoverTree_maxDescendentDistance ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_maxDescendentDistance node
    = and (map invariant_CoverTree_maxDescendentDistance $ stChildrenList node)
   && and (map (\dp -> distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

invariant_CoverTree_separating ::
    ( ValidCT exprat childC leafC dp
    , Elem (childC Bool) ~ Bool
    , Foldable (childC Bool)
    , Foldable (childC (Scalar dp))
    ) => CoverTree_ exprat childC leafC dp -> Bool
invariant_CoverTree_separating node
    = minimum ( P.filter (>0)
              $ map (\(x,y) -> distance (nodedp x) (nodedp y))
              $ cartesianProduct (toList $ children node) (toList $ children node)
              )
      >= sepdist node
   && and (map invariant_CoverTree_separating $ toList $ children node)
    where
        cartesianProduct :: [a] -> [b] -> [(a,b)]
        cartesianProduct xs ys = P.concatMap (\y -> map (\x -> (x,y)) xs) ys

{-
-- | FIXME:
-- This needs to be converted to use subhask notation.
property_leveled ::
    ( ValidCT exprat childC leafC dp
    , VG.Vector childC Bool
    , VG.Vector childC Int
    , VG.Vector childC (CoverTree_ exprat childC leafC dp)
    ) => CoverTree_ exprat childC leafC dp -> Bool
property_leveled node
    = VG.all (== VG.P.head xs) xs
   && VG.and (VG.map property_leveled $ children node)
    where
        xs = VG.map level $ children node
-}

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

-- | Sets the "maxDescendentDistance" parameter to the tightest bound possible.
-- This makes future queries of the tree more efficient---we can prune off more traversals, resulting in fewer distance computations.
--
-- FIXME:
-- This version uses a branch-and-bound recursion that is much faster than the previous naive implementation.
-- But it would be cooler (and faster?) if we used a find farthest neighbor method in the SpaceTree framework.
{-# INLINABLE setMaxDescendentDistance #-}
setMaxDescendentDistance ::
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
setMaxDescendentDistance ct = {-# SCC setMaxDescendentDistance #-} ct
    { children = children'
    , maxDescendentDistance = max leavesMaxDist $ go 0 (toList $ children')
    }
    where

        children' = fromList $ map setMaxDescendentDistance $ toList $ children ct

        leavesMaxDist = maximum $ map (distance (nodedp ct)) $ toList $ leaves ct

        go curmax []     = curmax
        go curmax (x:xs) = go curmax' xs
            where
                curmax' = if dist + maxDescendentDistance x <= curmax
                    then curmax
                    else go (maximum [dist,leavesMaxDist,curmax]) (toList $ children x)

                leavesMaxDist = maximum $ map (distance (nodedp ct)) $ toList $ leaves x

                dist = distance (nodedp x) (nodedp ct)

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
packCT :: forall exprat childC leafC dp.
    ( ValidCT exprat childC leafC dp
    ) => CoverTree_ exprat childC leafC dp
      -> CoverTree_ exprat childC leafC dp
packCT ct = {-# SCC packCT #-} snd $ go 0 ct
    where
        dpvec :: leafC dp
        dpvec = fromList $ stToList ct

        go !i !t = {-# SCC packCT_go #-} ( i',t
            { nodedp = dpvec!i
            , leaves = slice (i+1) (length $ leaves t) dpvec
            , children = fromList children'
            } )
            where
                (i',children') = {-# SCC mapAccumL #-} L.mapAccumL
                    go
                    (i+1+length (leaves t))
                    (toList $ children t)

-------------------------------------------------------------------------------
-- helper functions

coverdist :: (QuotientField (Scalar dp) Int, Real (Scalar dp)) =>
    CoverTree_ exprat childC leafC  dp -> Scalar dp
coverdist node = sepdist node*getExprat

sepdist :: forall exprat childC leafC dp. (QuotientField (Scalar dp) Int, Real (Scalar dp)) =>
    CoverTree_ exprat childC leafC dp -> Scalar dp
sepdist ct = getExprat**(fromIntegral $ level ct)

level2sepdist :: (QuotientField f r, Real f) => r -> f
level2sepdist i = getExprat**(fromIntegral i)

level2coverdist :: (QuotientField f r, Real f) => r -> f
level2coverdist i = level2sepdist (i+1)

dist2level_down :: forall exprat num.
    ({-KnownFrac exprat, -}Real num, QuotientField num Int) => Proxy exprat -> num -> Int
dist2level_down _ d = floor $ log d / log getExprat

dist2level_up :: forall exprat num.
    ({-KnownFrac exprat,-} Real num, QuotientField num Int) => Proxy exprat -> num -> Int
dist2level_up _ d = ceiling $ log d / log getExprat
