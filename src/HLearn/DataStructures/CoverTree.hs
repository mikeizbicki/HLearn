{-# LANGUAGE NoMonomorphismRestriction,DataKinds #-}

module HLearn.DataStructures.CoverTree
    ({- CoverTree
    , -}CoverTree'

    -- * unsafe
--     , ctmap
--     , unsafeMap
--     , recover
--     , trainct_insert
    , setNodeV
    , sepdistL

    -- * drawing
    , draw
    , draw'
    , IntShow (..)

    -- * QuickCheck properties
    , property_separating
    , property_covering
    , property_leveled
    , property_maxDescendentDistance
    )
    where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Control.DeepSeq
import Data.List hiding (insert)
import Data.Maybe
import Data.Monoid hiding ((<>))
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import Test.QuickCheck
import Debug.Trace

import Diagrams.Prelude hiding (distance,trace,query,connect)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>),(|>),numdp)
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor hiding (weight)
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..))

import HLearn.Models.Classifiers.Common
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- data types

-- type CoverTree dp = AddUnit (CoverTree' (2/1) V.Vector) () dp

data CoverTree' (base::Frac) nodeVvec tag dp = Node 
    { nodedp                :: !dp
    , sepdist               :: !(Ring dp)
    , weight                :: !(Ring dp)
    , numdp                 :: (Ring dp)
    , maxDescendentDistance :: (Ring dp)
    , children              :: !(V.Vector (CoverTree' base nodeVvec tag dp))
    , nodeV                 :: !(nodeVvec dp)
    , tag                   :: !tag
    }

deriving instance (Read (Ring dp), Read (nodeVvec dp), Read tag, Read dp) => Read (CoverTree' base nodeVvec tag dp)
deriving instance (Show (Ring dp), Show (nodeVvec dp), Show tag, Show dp) => Show (CoverTree' base nodeVvec tag dp)

instance (Eq dp, Eq (Ring dp)) => Eq (CoverTree' base nodeVvec tag dp) where
    ct1 == ct2 = sepdist ct1 == sepdist ct2 && nodedp ct1 == nodedp ct2

instance (Ord dp, Ord (Ring dp)) => Ord (CoverTree' base nodeVvec tag dp) where
    compare ct1 ct2 = compare (sepdist ct1) (sepdist ct2)
                   <> compare (nodedp ct1) (nodedp ct2)

instance (NFData dp,NFData (Ring dp),NFData tag) => NFData (CoverTree' base nodeVvec tag dp) where
    rnf ct = deepseq (nodedp ct) 
           $ deepseq (sepdist ct)
           $ deepseq (children ct)
           $ deepseq (tag ct)
           $ ()

instance 
    ( HasRing dp
    , MetricSpace dp
    , VG.Vector nodeVvec dp
    , SingI base
    ) => SpaceTree (CoverTree' base nodeVvec tag) dp
        where

    type LeafVector (CoverTree' base nodeVvec tag) = nodeVvec 

    {-# INLINABLE stMinDistance #-}
    {-# INLINABLE stMaxDistance #-}
    {-# INLINABLE stMinDistanceDpWithDistance #-}
    {-# INLINABLE stMaxDistanceDpWithDistance #-}
    {-# INLINABLE stMinDistanceDpFromDistance #-}
    {-# INLINABLE stMaxDistanceDpFromDistance #-}
    {-# INLINABLE stIsMinDistanceDpFartherThanWithDistance #-}
    {-# INLINABLE stIsMaxDistanceDpFartherThanWithDistance #-}
    {-# INLINABLE stChildren #-}
    {-# INLINABLE stChildren' #-}
    {-# INLINABLE stChildren_ #-}
    {-# INLINABLE stNode #-}
    {-# INLINABLE stNodeV #-}
    {-# INLINABLE stHasNode #-}
    {-# INLINABLE stIsLeaf #-}

    stMinDistanceWithDistance !ct1 !ct2 = 
        dist-(maxDescendentDistance ct1)-(maxDescendentDistance ct2) Strict.:!: dist
        where dist = distance (nodedp ct1) (nodedp ct2) 
    stMaxDistanceWithDistance !ct1 !ct2 = 
        dist+(maxDescendentDistance ct1)+(maxDescendentDistance ct2) Strict.:!: dist
        where dist = distance (nodedp ct1) (nodedp ct2) 

    stMinDistanceDpWithDistance !ct !dp = dist - maxDescendentDistance ct Strict.:!: dist
        where dist = distance (nodedp ct) dp
    stMaxDistanceDpWithDistance !ct !dp = dist + maxDescendentDistance ct Strict.:!: dist
        where dist = distance (nodedp ct) dp

    stIsMinDistanceDpFartherThanWithDistance !ct !dp !b = 
        case isFartherThanWithDistance (nodedp ct) dp (b+maxDescendentDistance ct) of
            Strict.Nothing -> Strict.Nothing
            Strict.Just dist -> Strict.Just $ dist

    stIsMaxDistanceDpFartherThanWithDistance !ct !dp !b = 
        isFartherThanWithDistance (nodedp ct) dp (b-maxDescendentDistance ct)

    stMinDistanceDpFromDistance !ct !dp !dist = dist-maxDescendentDistance ct
    stMaxDistanceDpFromDistance !ct !dp !dist = dist+maxDescendentDistance ct

    stChildren  = V.toList . children
    stChildren' = Strict.list2strictlist . V.toList . children
    stChildren_ = children
    stNodeV     = nodeV
    stNode      = nodedp
    stWeight    = weight
    stHasNode _ = True
--     stIsLeaf ct = Map.size (childrenMap ct) == 0
    stIsLeaf ct = V.null $ children ct
--     stIsLeaf ct = case children ct of   
--         Strict.Nil -> True
--         otherwise -> False

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 

setNodeV :: 
    ( MetricSpace dp
    , SingI base
    , Eq dp
    , VG.Vector nodeVvec dp
    ) => Int -> CoverTree' base nodeVvec tag dp -> CoverTree' base nodeVvec tag dp
setNodeV n ct = if stNumNodes ct > n
    then ct
        { children = fmap (setNodeV n) $ V.filter (not . stIsLeaf) $ children ct
        , nodeV = VG.fromList $ VG.toList $ fmap nodedp $ VG.filter stIsLeaf $ children ct
        }
    else ct
        { children = mempty
        , nodeV = VG.fromList $ stToList ct
        }

---------------------------------------

coverDist :: forall base nodeVvec tag dp.
    ( Fractional (Ring dp)
    , SingI base
    ) => CoverTree' base nodeVvec tag dp -> Ring dp
coverDist node = sepdist node*coverfactor 
    where
        coverfactor = fromSing (sing :: Sing base)

sepdist_child :: forall base nodeVvec tag dp. (SingI base, MetricSpace dp) => 
    CoverTree' base nodeVvec tag dp -> Ring dp
sepdist_child ct = sepdist ct/(fromSing (sing :: Sing base))

sepdist_parent :: forall base nodeVvec tag dp. (SingI base, MetricSpace dp) => 
    CoverTree' base nodeVvec tag dp -> Ring dp
sepdist_parent ct = sepdist ct*(fromSing (sing :: Sing base))

---------------------------------------
-- insertion as described in the paper

safeInsert :: forall base nodeVvec tag dp.
    ( MetricSpace dp
    , Ord (Ring dp)
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    , Floating (Ring dp)
    , Monoid tag
    , SingI base
    ) => CoverTree' base nodeVvec tag dp -> Weighted dp -> CoverTree' base nodeVvec tag dp
safeInsert node (0,_) = node
safeInsert !node !(w,dp) = case insert node (w,dp) of
    Strict.Just x -> x
    Strict.Nothing -> trace "insert.Nothing" $ Node
        { nodedp    = nodedp node
        , sepdist   = roundup (sing::Sing base) dist
        , weight    = 0 -- weight node
        , numdp     = 0 + Strict.sum (fmap numdp children')
        , tag       = mempty
        , children  = V.fromList $ Strict.strictlist2list children'
        , nodeV     = mempty
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }
        where
            children' :: Strict.List (CoverTree' base nodeVvec tag dp)
            children' = (growct node (rounddown (sing::Sing base) dist))
                      :.(Node 
                            { nodedp    = dp 
                            , weight    = w
                            , numdp     = w
                            , sepdist   = rounddown (sing::Sing base) dist
                            , tag       = mempty
                            , children  = mempty
                            , nodeV     = mempty
                            , maxDescendentDistance = 0
                            })
                      :.Strict.Nil

            dist = distance (nodedp node) dp


insert :: forall base nodeVvec tag dp.
    ( MetricSpace dp
    , Ord (Ring dp)
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    , Fractional (Ring dp)
    , Monoid tag
    , SingI base
    ) => CoverTree' base nodeVvec tag dp -> Weighted dp -> Strict.Maybe (CoverTree' base nodeVvec tag dp)
insert !node !(!w,!dp) = if isFartherThan dp (nodedp node) (sepdist node)
    then Strict.Nothing
    else Strict.Just $ Node
        { nodedp    = nodedp node
        , sepdist   = sepdist node
        , weight    = weight node
        , numdp     = weight node + Strict.sum (fmap numdp children')
        , tag       = tag node
        , children  = V.fromList $ Strict.strictlist2list children'
        , nodeV     = mempty
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }

    where 
        children' :: List (CoverTree' base nodeVvec tag dp)
        children' = go $ Strict.list2strictlist $ V.toList $ children node

        go Nil = (Node 
                    { nodedp = dp
                    , sepdist = sepdist_child node
                    , weight = w
                    , numdp = w
                    , maxDescendentDistance = 0
                    , children = mempty
                    , nodeV = mempty
                    , tag = mempty
                    })
               :.Nil
        go (x:.xs) = if isFartherThan (nodedp x) dp (sepdist x)
            then x:.go xs
            else case insert x (w,dp) of
                Strict.Just x' -> x':.xs
--                 Strict.Nothing -> error "Nothing" 

        {-childrenMap' = if {-# SCC cond #-} hasInsert
            then {-# SCC cond_then #-} Map.insert key val $ childrenMap node
            else {-# SCC cond_else #-} Map.insert dp (Node 
                { nodedp  = dp 
                , sepdist = sepdist node/coverfactor
                , weight  = w 
                , numdp   = w
                , maxDescendentDistance = 0
                , childrenMap  = mempty
                , childrenList = mempty
                , tag          = mempty
                }) $ childrenMap node

        viableChildren = Map.filter (\subtree -> not $ isFartherThan (nodedp subtree) dp (coverDist subtree)) $ childrenMap node
        childrenInserts = Map.map (\tree -> insert tree (w,dp)) viableChildren

        insertables = Map.filterWithKey filtergo childrenInserts
            where
                filtergo _ Nothing   = False
                filtergo _ (Just _)  = True

        (key,Just val):xs = {-sortBy sortgo $-} Map.assocs insertables
            where
                sortgo (_,Just v1) (_,Just v2) = compare (Map.size $ childrenMap v1) (Map.size $ childrenMap v2)

        hasInsert = Map.size insertables > 0
        coverfactor = fromSing (sing :: Sing base)
-}
insertBatch :: forall base nodeVvec tag dp.
    ( MetricSpace dp
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Monoid tag
    , SingI base
    ) => [dp] -> CoverTree' base nodeVvec tag dp
insertBatch ((!dp):dps) = go dps $ Node 
    { nodedp    = dp
    , sepdist   = 0
    , weight    = 1
    , numdp     = 1
    , tag       = mempty
    , children  = mempty
    , nodeV     = mempty
    , maxDescendentDistance = 0
    }
    where
        go [] tree = tree
        go (x:xs) tree = go xs $ safeInsert tree (1,x)

-------------------------------------------------------------------------------
-- algebra

-- instance F.Foldable (CoverTree' tag) where
--     foldr f i ct = if Map.size (childrenMap ct) == 0
--         then f (nodedp ct) i
--         else foldr (\ct' i' -> F.foldr f i' ct') i' (Map.elems $ childrenMap ct)
--         where
--             i' = if nodedp ct `Map.member` childrenMap ct
--                 then i
--                 else f (nodedp ct) i

instance (HasRing dp) => HasRing (CoverTree' base nodeVvec tag dp) where
    type Ring (CoverTree' base nodeVvec tag dp) = Ring dp

instance
    ( MetricSpace dp
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    , Ord dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Fractional (Ring dp)
    , Monoid tag
    , SingI base
    , Show dp
    , Show tag
    , Show (Ring dp)
    ) => Semigroup (CoverTree' base nodeVvec tag dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = case ctmerge' ct1' ct2' of
        Just (ct, []) -> ct
        Just (ct, xs) -> foldl' (<>) ct xs
        Nothing -> (growct ct1' (roundup (sing::Sing base) $ distance (nodedp ct1') (nodedp ct2'))) <> ct2'
        where
            ct1' = growct ct1 maxsepdist
            ct2' = growct ct2 maxsepdist
            maxsepdist = maximum [sepdist ct1, sepdist ct2, 1]

ctmerge' :: forall base nodeVvec tag dp.
    ( Ord (Ring dp)
    , Ord dp 
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    , Floating (Ring dp)
    , Monoid tag
    , SingI base
    , MetricSpace dp
    , Show dp
    , Show (Ring dp)
    ) => CoverTree' base nodeVvec tag dp 
      -> CoverTree' base nodeVvec tag dp 
      -> Maybe (CoverTree' base nodeVvec tag dp, [CoverTree' base nodeVvec tag dp])
ctmerge' ct1 ct2 = assert (sepdist ct2 >= sepdist ct1) ("sepdist ct2 >= sepdist ct1:" ++ show (sepdist ct1) ++","++show(sepdist ct2)) $
  if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Nothing
    else Just 
        ( 
          flip safeInsert (stNodeW ct2) $ 
          ct1
            { children = V.fromList $ Strict.strictlist2list children'
            , maxDescendentDistance 
                = sepdist_parent ct1
--                 = maximum $ map (distance (nodedp ct1)) $ (stDescendents ct2++stDescendents ct1)
            }
        , invalidchildren++invalid_newleftovers
        )
    where
        children' = Strict.list2strictlist $ Map.elems childrenMap' 

        childrenMap ct = Map.fromList $ map (\v -> (nodedp v,v)) $ stChildren ct

        childrenMap' = newchildren `Map.union` Map.fromList 
            (map (\x -> (nodedp x,growct x $ sepdist_child ct1)) valid_newleftovers)


        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (sepdist ct1)
        (validchildren,invalidchildren) = partition validchild $ Map.elems $ childrenMap ct2

        (newchildren,newleftovers) = 
            go (childrenMap ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = partition validchild newleftovers

        go (childmap,leftovers) [] = 
            (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = 
            case filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge' v x)) $ Map.assocs childmap of
                [] -> go 
                    ( Map.insert (nodedp x) (x { sepdist = sepdist_child ct1 }) childmap
                    , leftovers
                    ) xs

                (old, Just (new,leftovers')):ys ->
                    go ( Map.insert (nodedp new) (new { sepdist = sepdist_child ct1 })
                         $ Map.delete old childmap
                       , leftovers'++leftovers
                       ) xs

assert :: Bool -> String -> a -> a
assert b str a = (if b then id else trace ("assert: "++str)) a
-- assert b str a = if not b
--     then error $ "assert: "++str
--     else a

growct :: forall base nodeVvec tag dp.
    ( Fractional (Ring dp)
    , Ord (Ring dp)
    , Monoid (nodeVvec dp)
    , VG.Vector nodeVvec dp
    
    , Monoid tag
    , SingI base
    , MetricSpace dp
    ) => CoverTree' base nodeVvec tag dp -> Ring dp -> CoverTree' base nodeVvec tag dp
growct ct d = if sepdist ct==0 || stIsLeaf ct 
    then ct { sepdist=d }
    else if d > sepdist ct
        then growct (Node
            { nodedp    = nodedp ct
            , sepdist   = sepdist ct*coverfactor
            , weight    = 0 -- weight ct
            , numdp     = numdp ct
            , tag       = mempty
            , children  = V.fromList $ Strict.strictlist2list $ ct:.Strict.Nil
            , nodeV     = mempty
            , maxDescendentDistance = maxDescendentDistance ct
            }
            ) d
        else ct
    where
        coverfactor = fromSing (sing :: Sing base)


{-# INLINE roundup #-}
roundup :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
roundup s d = rounddown s $ d * coverfactor
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINE rounddown #-}
rounddown :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
rounddown _ d
    | d == 1 = 1
    | d >  1 = goup 1
    | d <  1 = godown 1
    where
        godown i = if i > d 
            then godown (i/coverfactor)
            else i
        goup i = if i<d
            then goup (i*coverfactor)
            else i
        coverfactor = fromSing (sing :: Sing base)
-- rounddown _ d = coverfactor^^(floor $ log d / log coverfactor :: Int)
--     where
--         coverfactor = fromSing (sing :: Sing base)

sepdistL :: Ord (Ring dp) => CoverTree' base nodeVvec tag dp -> [Ring dp]
sepdistL ct = Set.toList $ Set.fromList
    $ sepdist ct : (mconcat $ map sepdistL $ VG.toList $ children ct)
-- sepdistL ct = Set.toList $ go ct
--     where
--         go ct = (Set.singleton $ sepdist ct) 
--              <> (Set.fromList $ VG.toList $ fmap go $ children ct)
--         go :: CoverTree' base nodeVvec tag dp -> [Ring dp]
--         go ct=(Set.fromList $ foldr (<>) mempty $ VG.toList $ fmap go $ children ct)

---------------------------------------

{-
recover ct = foldl' safeInsert ct' xs
    where
        (ct', xs) = recover' ct

recover' :: forall base nodeVvec tag dp.
    ( MetricSpace dp
    
    , SingI base
    ) => CoverTree' base nodeVvec tag dp -> (CoverTree' base nodeVvec tag dp, [Weighted dp])
recover' ct = (ct', failed)
    where
        ct' = ct
            { childrenMap = pass
            , childrenList = Map.elems pass
            }
        
        (fail,pass) = Map.partition 
            (\c -> not $ isFartherThan (nodedp ct) (nodedp c) (coverDist ct)) 
            (childrenMap ct)
        

        failed = concatMap stToListW $ Map.elems fail 

-- unsafeMap :: forall base nodeVvec tag dp1 dp2.
--     ( MetricSpace dp2
--     , Ring dp1 ~ Ring dp2
--     2
--     , SingI base
--     ) => (dp1 -> dp2) -> AddUnit (CoverTree' base) tag dp1 -> AddUnit (CoverTree' base) tag dp2
unsafeMap f Unit = Unit
unsafeMap f (UnitLift ct) = UnitLift $ unsafeMap' f ct

unsafeMap' :: forall base nodeVvec tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    2
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base nodeVvec tag dp1 -> CoverTree' base nodeVvec tag dp2
unsafeMap' f ct = Node
    { nodedp = nodedp'
    , weight = weight ct
    , numdp = numdp ct
    , sepdist = sepdist ct
    , tag = tag ct
    , childrenMap = childrenMap'
    , childrenList = childrenList'
--     , maxDescendentDistance = maxDescendentDistance ct
    , maxDescendentDistance = maximum $ 0:map (\c -> distance (nodedp c) nodedp' + maxDescendentDistance c) childrenList'
    }
    where
        nodedp' = f $ nodedp ct
        childrenMap' = Map.fromList $ map (\c -> (nodedp c,c)) $ map (unsafeMap' f) $ childrenList ct 
        childrenList' = Map.elems childrenMap'

ctmap f Unit = Unit
ctmap f (UnitLift ct) = UnitLift $ ctmap' f ct

ctmap' :: forall base nodeVvec tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    , Floating (Ring dp1)
    2
    , Monoid tag
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base nodeVvec tag dp1 -> CoverTree' base nodeVvec tag dp2
ctmap' f ct = recover $ unsafeMap' f ct
 

implicitChildrenMap ct = Map.union (childrenMap ct) (Map.singleton (nodedp ct) $ ct
    { nodedp = nodedp ct
    , sepdist = sepdist_child ct
    , weight = 0
    , numdp = 0
    , tag = tag ct
    , childrenMap = mempty
    , childrenList = mempty
    , maxDescendentDistance = 0
    })

extractLeaf :: forall base nodeVvec tag dp.
    ( MetricSpace dp 
    
    , SingI base
    ) => CoverTree' base nodeVvec tag dp -> (Weighted dp, Maybe (CoverTree' base nodeVvec tag dp))
extractLeaf ct = if stIsLeaf ct
    then (stNodeW ct, Nothing)
    else (leaf, Just $ ct
            { childrenMap = childrenMap'
            , childrenList = Map.elems childrenMap'
            }
         )
        where
            (leaf,c) = extractLeaf . head . Map.elems $ childrenMap ct

            childrenMap' = case c of
                Nothing -> Map.fromList $ tail $ Map.toList $ childrenMap ct
                Just c' -> Map.fromList $ (nodedp c', c'):(tail $ Map.toList $ childrenMap ct)
-}

-- setLeafSize :: (MetricSpace dp, SingI base) => Int -> CoverTree' base nodeVvec tag dp -> CoverTree' base nodeVvec tag dp
-- setLeafSize n ct = if stNumNodes ct < n
--     then ct { children = fmap singleton $ Strict.list2strictlist $ stToListW ct } 
--     else ct { children = fmap (setLeafSize n) $ children ct }
--     where
-- --         singleton :: Weighted dp -> CoverTree' base nodeVvec tag dp
--         singleton (w,dp) = Node
--             { nodedp = dp
--             , weight = w
--             , numdp = w
--             , sepdist = sepdist_child ct
--             , maxDescendentDistance = 0
--             , children = mempty
--             , tag = tag ct
--             }
             

-------------------------------------------------------------------------------
-- training

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Ord dp
    , Show dp
    , Show tag
    , Show (Ring dp)
    , VG.Vector nodeVvec dp
    , Monoid (nodeVvec dp)
    , Monoid tag
    , SingI base
    ) => HomTrainer (AddUnit (CoverTree' base nodeVvec) tag dp) 
        where
    type Datapoint (AddUnit (CoverTree' base nodeVvec) tag dp) = dp

    {-# INLINE train1dp #-}
    train1dp dp = UnitLift $ Node 
        { nodedp    = dp
        , sepdist   = 0
        , weight    = 1
        , numdp     = 1
        , tag       = mempty
        , children  = mempty
        , nodeV     = mempty
        , maxDescendentDistance = 0
        }

    {-# INLINABLE train #-}
    train = UnitLift . insertBatch . F.toList

trainct_insert = UnitLift . insertBatch . F.toList

-------------------------------------------------------------------------------
-- tests

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Ord dp
    , Show dp
    , Show tag
    , Show (Ring dp)
    , VG.Vector nodeVvec dp
    , Monoid (nodeVvec dp)
    , Monoid tag
    , SingI base
    , dp ~ (Double,Double)
--     ( VG.Vector nodeVvec (Double,Double)
--     , Monoid (nodeVvec (Double,Double))
--     , SingI base
--     , Monoid tag
    ) => Arbitrary (CoverTree' base nodeVvec tag (Double,Double)) 
        where
    arbitrary = do
        num :: Int <- choose (1,100)
--         xs <- replicateM num arbitrary
        xs <- replicateM num $ do
            x <- arbitrary
            y <- arbitrary
--             x <- choose (-2^^5,2^^5)
--             y <- choose (-2^^5,2^^5)
            return (x,y)
        return $ unUnit $ train xs 


-- property_all :: CoverTree (Double,Double) -> Bool
property_all ct = and $ map (\x -> x ct)
    [ property_covering
    , property_leveled
    , property_separating
    , property_maxDescendentDistance
    ]

-- property_covering :: (MetricSpace dp) => CoverTree dp -> Bool
property_covering Unit = True
property_covering (UnitLift node) = if not $ stIsLeaf node
    then F.maximum (fmap (distance (nodedp node) . nodedp) $ children node) < coverDist node 
      && F.and (fmap (property_covering . UnitLift) $ children node)
    else True

-- property_leveled  :: MetricSpace dp => CoverTree dp -> Bool
property_leveled (Unit) = True
property_leveled (UnitLift node)
    = VG.all (\val -> val - VG.head xs < 0.0000001) xs
   && VG.and (fmap (property_leveled . UnitLift) $ children node)
    where
        xs = fmap sepdist $ children node

-- property_separating  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_separating Unit = True
property_separating (UnitLift node) = if VG.length (children node) > 1 
    then F.foldl1 min ((mapFactorial stMaxDistance) $ children node) > sepdist_child node
      && F.and (fmap (property_separating . UnitLift) $ children node)
    else True
    where
        mapFactorial :: (a -> a -> b) -> V.Vector a -> V.Vector b
        mapFactorial f = VG.fromList . Strict.strictlist2list . mapFactorial' f . Strict.list2strictlist . VG.toList
        mapFactorial' :: (a -> a -> b) -> List a -> List b
        mapFactorial' f xs = go xs Nil
            where
                go Nil ys = ys
                go (x:.xs) ys = go xs (fmap (f x) xs `mappend` ys)
-- property_separating (UnitLift node) = if Strict.length (children node) > 1 
--     then F.foldl1 min ((mapFactorial stMaxDistance) $ children node) > sepdist_child node
--       && F.and (fmap (property_separating . UnitLift) $ children node)
--     else True
--     where
--         mapFactorial :: (a -> a -> b) -> List a -> List b
--         mapFactorial f xs = go xs Nil
--             where
--                 go Nil ys = ys
--                 go (x:.xs) ys = go xs (fmap (f x) xs `mappend` ys)

-- property_maxDescendentDistance  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) = if stIsLeaf node
    then True
    else and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
      && and (map (\dp -> weight node == 1 
                       || distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

-- property_validmerge :: 
--     (CoverTree (Double,Double) -> Bool) -> CoverTree (Double,Double) -> CoverTree (Double,Double) -> Bool
property_validmerge prop (UnitLift ct1) (UnitLift ct2) = prop . UnitLift $ ct1 <> ct2

-- property_lossless :: [(Double,Double)] ->  Bool
-- property_lossless [] = True
-- property_lossless xs = Set.fromList xs == dpSet ct
--     where
--         UnitLift ct = train xs :: AddUnit (CoverTree' (2/1)) () (Double,Double)
-- 
--         dpSet :: (Ord dp) => CoverTree' base nodeVvec tag dp -> Set.Set dp
--         dpSet = Set.fromList . dpList
--             where
--                 dpList :: CoverTree' base nodeVvec tag dp -> [dp]
--                 dpList node = nodedp node:(Strict.concat . fmap dpList . children node)

-- property_numdp :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_numdp Unit = True
property_numdp (UnitLift node) = numdp node == sum (map fst $ stToListW node)


---------------------------------------

instance MkCentroid (Double,Double) where
    mkCentroid (a1,a2) (b1,b2) = ((a1+b1)/2,(a2+b2)/2)

randL :: Int -> IO [(Double,Double)]
randL n = replicateM n $ do
    x <- randomRIO (-100,100)
    y <- randomRIO (-100,100)
    return (fromIntegral (x :: Int), fromIntegral (y :: Int))

dp1 = (1,1) :: (Double,Double)
dp2 = (2,1) :: (Double,Double)
-- m1 = unUnit (train1dp dp1 :: CoverTree (Double,Double))
-- m2 = unUnit (train1dp dp2 :: CoverTree (Double,Double))

ys :: [(Double,Double)]
-- ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
-- ys = [(0,0),(0,10),(10,10),(10,0),(10,-10),(0,-10),(-10,-10),(-10,0),(-10,10)]
ys = [(0,0),(0,10),(8,8),(10,0),(8,-8),(0,-10),(-8,-8),(-10,0),(-8,8)]
-- my = train ys :: CoverTree (Double,Double)
-- my2 = train $ take 3 ys :: CoverTree (Double,Double)
-- my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
-- my' = train ys' :: CoverTree (Double,Double)
-- my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
-- zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(20,10),(22,21)]
-- mz = train zs :: CoverTree (Double,Double)
-- mz = prunect $ insertBatch zs

-- mq = mz `mappend` my

grid :: Int -> Int -> [(Double,Double)]
grid n 2 = take n [(x,y) | x <- [1..w], y <- [1..w]]
    where
        n' = fromIntegral n
        w = fromIntegral $ ceiling $ n' ** (1/2)

gs = grid 9 2
gs' i = take i gs
-- mg = train gs :: CoverTree (Double,Double)
-- mg' i = train $ gs' i :: CoverTree (Double,Double)

-- mg3 = train1dp (1,1) `mappend` ( train1dp (1,2) `mappend` train1dp (1,3) ) :: CoverTree (Double,Double)

-------------------------------------------------------------------------------
-- diagrams

drawT ct1 ct2 = draw ct1 
            ||| (text "<>" <> strutX 1.5)
            ||| draw ct2 
            ||| (text "=" <> strutX 1) 
            ||| (draw $ ct1 `mappend` ct2)

draw node = draw' 0 node
draw' depth tree = mkConnections $ 
                   (named (label++show depth) $ fontSize 0.5 $ 
                        (
                             (text label <> strutY 0.5) 
                         === (text (show (sepdist tree)) <> strutY 0.5) 
--                          === (text (show (maxDescendentDistance tree)) <> strutY 0.5)
                        ) 
                          <> circle 1 # fc nodecolor) 
               === (pad 1.05 $ centerName (label++show (depth+1)) $ 
                   F.foldr (|||) mempty $ fmap (draw' (depth+1)) $ children tree)
                
    where
        label = intShow $ nodedp tree
        nodecolor = if weight tree > 0
            then red
            else lightblue

        mkConnections = undefined
--          connect (label++show depth) (label++show (depth+1)) 
--          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (fmap nodedp $ children tree))
            
justdouble :: Maybe Double -> String
justdouble Nothing = "0"
justdouble (Just x) = show x

apList :: List (a -> a) -> a -> a
apList Strict.Nil a = a
apList (x:.xs) a = apList xs (x a)

centerName name = withName name $ \b a -> moveOriginTo (location b) a

connect n1 n2
    = withName n1 $ \b1 ->
      withName n2 $ \b2 ->
        atop ((location b1 ~~ location b2) # lc green # lw 0.03)
--          ((location b1 ~~ location b2) # lc green # lw 0.03)


class IntShow a where
    intShow :: a -> String

instance IntShow (Double,Double) where
    intShow (x,y) = show (floor x::Int,floor y::Int)

instance (VG.Vector (L2 v) r, RealFrac r) => IntShow (L2 v r) where
    intShow v = show (map floor $ VG.toList v)

instance (IntShow attr,Show label) => IntShow (MaybeLabeled label attr) where
    intShow dp = "("++label++","++intShow (getAttributes dp)++")"
        where
            label = case getLabel dp of
                Just x -> show x
                Nothing -> "_"
