{-# LANGUAGE NoMonomorphismRestriction,DataKinds #-}

module HLearn.DataStructures.CoverTree
    ( CoverTree
    , CoverTree'

    -- * unsafe
    , ctmap
    , unsafeMap
    , recover

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

import Diagrams.Prelude hiding (distance,trace,query)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>),(|>),numdp)
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor hiding (weight)
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch

import HLearn.Models.Classifiers.Common
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = AddUnit (CoverTree' (2/1)) () dp

data CoverTree' (base::Frac) tag dp = Node 
    { nodedp                :: !dp
    , sepdist               :: !(Ring dp)
    , weight                :: !(Ring dp)
    , numdp                 :: !(Ring dp)
    , maxDescendentDistance :: !(Ring dp)
    , childrenMap           :: !(Map.Map dp (CoverTree' base tag dp)) 
    , childrenList          :: [CoverTree' base tag dp] -- intentionally lazy
    , tag                   :: !tag
    }

deriving instance (Read (Ring dp), Read tag, Read dp, Ord dp) => Read (CoverTree' base tag dp)
deriving instance (Show (Ring dp), Show tag, Show dp, Ord dp) => Show (CoverTree' base tag dp)

instance (Eq dp, Eq (Ring dp)) => Eq (CoverTree' base tag dp) where
    ct1 == ct2 = sepdist ct1 == sepdist ct2 && nodedp ct1 == nodedp ct2

instance (Ord dp, Ord (Ring dp)) => Ord (CoverTree' base tag dp) where
    compare ct1 ct2 = compare (sepdist ct1) (sepdist ct2)
                   <> compare (nodedp ct1) (nodedp ct2)

instance (NFData dp,NFData (Ring dp),NFData tag) => NFData (CoverTree' base tag dp) where
    rnf ct = deepseq (nodedp ct) 
           $ deepseq (sepdist ct)
           $ deepseq (childrenMap ct)
           $ deepseq (tag ct)
           $ ()

instance 
    ( HasRing dp
    , MetricSpace dp
    , Ord dp
    , SingI base
    ) => SpaceTree (CoverTree' base tag) dp
        where
    {-# INLINABLE stMinDistance #-}
    {-# INLINABLE stMaxDistance #-}
    {-# INLINABLE stMinDistanceDpWithDistance #-}
    {-# INLINABLE stMaxDistanceDpWithDistance #-}
    {-# INLINABLE stMinDistanceDpFromDistance #-}
    {-# INLINABLE stMaxDistanceDpFromDistance #-}
    {-# INLINABLE stChildren #-}
    {-# INLINABLE stNode #-}
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

    stChildren  =  childrenList
    stNode      = nodedp
    stWeight    = weight
    stHasNode _ = True
    stIsLeaf ct = Map.size (childrenMap ct) == 0

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 

---------------------------------------

coverDist :: forall base tag dp.
    ( Fractional (Ring dp)
    , SingI base
    ) => CoverTree' base tag dp -> Ring dp
coverDist node = sepdist node*coverfactor 
    where
        coverfactor = fromSing (sing :: Sing base)

sepdist_child :: forall base tag dp. (SingI base, MetricSpace dp) => 
    CoverTree' base tag dp -> Ring dp
sepdist_child ct = sepdist ct/(fromSing (sing :: Sing base))

sepdist_parent :: forall base tag dp. (SingI base, MetricSpace dp) => 
    CoverTree' base tag dp -> Ring dp
sepdist_parent ct = sepdist ct*(fromSing (sing :: Sing base))

---------------------------------------
-- insertion as described in the paper

safeInsert :: forall base tag dp.
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Floating (Ring dp)
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> Weighted dp -> CoverTree' base tag dp
safeInsert !node !(w,dp) = case insert node (w,dp) of
    Just x -> x
    Nothing -> Node
        { nodedp    = nodedp node
        , sepdist   = roundup (sing::Sing base) dist
        , weight    = 0 -- weight node
        , numdp     = 0 + sum (map numdp childrenList')
        , tag       = mempty
        , childrenMap = childrenMap'
        , childrenList = childrenList'
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }
        where
            childrenList' = Map.elems childrenMap'
            childrenMap' = Map.fromList 
                [ (nodedp node, growct node (rounddown (sing::Sing base) dist))
                , (dp, Node 
                    { nodedp    = dp 
                    , weight    = w
                    , numdp     = w
                    , sepdist   = rounddown (sing::Sing base) dist
                    , tag       = mempty
                    , childrenMap = mempty
                    , childrenList = []
                    , maxDescendentDistance = 0
                    })
                ]

            dist = distance (nodedp node) dp


insert :: forall base tag dp.
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Fractional (Ring dp)
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> Weighted dp -> Maybe (CoverTree' base tag dp)
insert !node !(!w,!dp) = if isFartherThan dp (nodedp node) (sepdist node)
    then Nothing
    else seq justnode $ Just justnode
    where 
        justnode = Node
            { nodedp    = nodedp node
            , sepdist   = sepdist node
            , weight    = weight node
            , numdp     = weight node + sum (map numdp childrenList')
            , tag       = tag node
            , childrenMap = childrenMap'
            , childrenList = childrenList'
            , maxDescendentDistance = max
                (maxDescendentDistance node)
                (distance (nodedp node) dp)
            }

        childrenList' = Map.elems childrenMap'
        childrenMap' = if hasInsert
            then Map.insert key val $ childrenMap node
            else Map.insert dp (Node 
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

insertBatch :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Monoid tag
    , SingI base
    ) => [dp] -> CoverTree' base tag dp
insertBatch ((!dp):dps) = go dps $ Node 
    { nodedp    = dp
    , sepdist   = 0
    , weight    = 1
    , numdp     = 1
    , tag       = mempty
    , childrenMap = mempty
    , childrenList = []
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

instance (HasRing dp) => HasRing (CoverTree' base tag dp) where
    type Ring (CoverTree' base tag dp) = Ring dp

instance
    ( MetricSpace dp
    , Ord dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Fractional (Ring dp)
    , Monoid tag
    , SingI base
    ) => Semigroup (CoverTree' base tag dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = unsafeMerge ct1 ct2

growct :: forall base tag dp.
    ( Fractional (Ring dp)
    , Ord (Ring dp)
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> Ring dp -> CoverTree' base tag dp
growct ct d = if sepdist ct==0 || Map.size (childrenMap ct)==0
    then ct { sepdist=d }
    else if d > sepdist ct
        then growct (Node
            { nodedp    = nodedp ct
            , sepdist   = sepdist ct*coverfactor
            , weight    = 0 -- weight ct
            , numdp     = numdp ct
            , tag       = mempty
            , childrenMap = Map.singleton (nodedp ct) ct 
            , childrenList = [ct]
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
roundup s d = rounddown s d * coverfactor
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINE rounddown #-}
rounddown :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
rounddown _ d = coverfactor^^(floor $ log d / log coverfactor :: Int)
    where
        coverfactor = fromSing (sing :: Sing base)

---------------------------------------

recover :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , SingI base
    ) => CoverTree' base tag dp -> (CoverTree' base tag dp, [Weighted dp])
recover ct = (ct', failed)
    where
        ct' = ct
            { childrenMap = pass
            , childrenList = Map.elems pass
            }
        
        (fail,pass) = Map.partition (\c -> not $ isFartherThan (nodedp ct) (nodedp c) (coverDist ct)) (childrenMap ct)
        

        failed = concatMap stToListW $ Map.elems fail 

unsafeMap :: forall base tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    , Ord dp2
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base tag dp1 -> CoverTree' base tag dp2
unsafeMap f ct = Node
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
        childrenMap' = Map.fromList $ map (\c -> (nodedp c,c)) $ map (unsafeMap f) $ childrenList ct 
        childrenList' = Map.elems childrenMap'

ctmap :: forall base tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    , Floating (Ring dp1)
    , Ord dp2
    , Monoid tag
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base tag dp1 -> CoverTree' base tag dp2
ctmap f ct = foldl' safeInsert ct' xs
    where
        (ct',xs) = recover $ unsafeMap f ct

unsafeJoin :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , SingI base
    ) => CoverTree' base tag (CoverTree' base tag dp) -> CoverTree' base tag dp
unsafeJoin ct = Node
    { nodedp = nodedp $ nodedp ct
    , sepdist = sepdist ct
    , weight = weight ct
    , numdp = numdp ct
    , tag = tag ct
    , childrenMap = childrenMap'
    , childrenList = childrenList'
    , maxDescendentDistance = infinity
    }
    where
        childrenMap' = Map.fromList $ map (\c -> (nodedp c, c)) childrenList'
        childrenList' = map unsafeJoin $ childrenList ct

unsafeMerge :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , Monoid tag
    , Floating (Ring dp)
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp -> CoverTree' base tag dp
unsafeMerge ct1 ct2 = 
    if coverDist ct1 == coverDist ct2
        then unsafeMerge_sameLevel ct1 ct2 
        else unsafeMerge_difLevel ctmax ctmin
    where
        (ctmin,ctmax) = if sepdist ct1 > sepdist ct2
            then (ct2,ct1)
            else (ct1,ct2)

unsafeMerge_difLevel :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , Monoid tag
    , Floating (Ring dp)
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp -> CoverTree' base tag dp
unsafeMerge_difLevel ct1 ct2 = case eligibleChildren of
    (d,c):cs -> let childrenMap' = Map.insert (nodedp c) (unsafeMerge c ct2) (childrenMap ct1) in ct1
        { childrenMap = childrenMap'
        , childrenList = Map.elems childrenMap'
        , maxDescendentDistance = max 
            (maxDescendentDistance ct1) 
            (maxDescendentDistance ct2 + distance (nodedp ct1) (nodedp ct2))
        }

    [] -> let childrenMap' = Map.insert (nodedp c') c' $ childrenMap ct1
              c' = Node
                { nodedp = nodedp ct2
                , weight = 0
                , numdp = numdp ct2
                , sepdist = sepdist_child ct1
                , tag = tag ct2
                , childrenMap = Map.singleton (nodedp ct2) ct2
                , childrenList = [ct2]
                , maxDescendentDistance = maxDescendentDistance ct2
                }
        in ct1
            { childrenMap = childrenMap'
            , childrenList = Map.elems childrenMap'
            , maxDescendentDistance = max 
                (maxDescendentDistance ct1) 
                (distance (nodedp ct1) (nodedp c'))
            }
    where
        eligibleChildren = filter (\(d,c) -> d<sepdist_child ct1) 
                         $ map (\c -> (distance (nodedp ct2) (nodedp c),c)) (childrenList ct1)

unsafeMerge_sameLevel :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , Monoid tag
    , Floating (Ring dp)
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp -> CoverTree' base tag dp
unsafeMerge_sameLevel ct1 ct2 = case isFartherThanWithDistance (nodedp ct1) (nodedp ct2) (sepdist ct1) of

    Strict.Nothing -> Node
        { nodedp = dp
        , weight = w
        , numdp = w + sum (map numdp then_childrenList')
        , sepdist = roundup (sing::Sing base) $ distance (nodedp ct1) (nodedp ct2)
        , maxDescendentDistance = maxDescendentDistance ct1 + distance (nodedp ct1) dp
        , tag = tag ct2
        , childrenMap = then_childrenMap'
        , childrenList = then_childrenList'
        }
        where
            ((w,dp),c) = extractLeaf ct2

            then_childrenList' = Map.elems then_childrenMap'
            then_childrenMap' = case c of
                Just c -> Map.fromList [(nodedp c, c), (nodedp ct1, ct1)]
                Nothing -> Map.fromList [(nodedp ct1,ct1)]

    Strict.Just dist -> flip safeInsert (stNodeW ct2) $ ct1
        { childrenMap = else_childrenMap'
        , childrenList = Map.elems else_childrenMap'
        , maxDescendentDistance = max 
            (maxDescendentDistance ct1) 
            (maxDescendentDistance ct2 + dist)
        }
        where
            else_childrenMap' = childrenMap ct1 `Map.union` childrenMap ct2

extractLeaf :: forall base tag dp.
    ( MetricSpace dp 
    , Ord dp
    , SingI base
    ) => CoverTree' base tag dp -> (Weighted dp, Maybe (CoverTree' base tag dp))
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

-------------------------------------------------------------------------------
-- training

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Ord dp
    ) => HomTrainer (CoverTree dp) 
        where
    type Datapoint (CoverTree dp) = dp
    {-# INLINE train1dp #-}
    train1dp dp = UnitLift $ Node 
        { nodedp    = dp
        , sepdist   = 0
        , weight    = 1
        , numdp     = 1
        , tag       = mempty
        , childrenMap = mempty
        , childrenList = mempty
        , maxDescendentDistance = 0
        }

    {-# INLINABLE train #-}
    train = UnitLift . insertBatch . F.toList

-------------------------------------------------------------------------------
-- tests

instance Arbitrary (CoverTree (Double,Double)) where
    arbitrary = do
        num :: Int <- choose (1,100)
--         xs <- replicateM num arbitrary
        xs <- replicateM num $ do
--             x <- arbitrary
--             y <- arbitrary
            x <- choose (-2^^5,2^^5)
            y <- choose (-2^^5,2^^5)
            return (x,y)
        return $ UnitLift $ unUnit $ train xs 

property_all :: CoverTree (Double,Double) -> Bool
property_all ct = and $ map (\x -> x ct)
    [ property_covering
    , property_leveled
    , property_separating
    , property_maxDescendentDistance
    ]

property_covering :: MetricSpace dp => CoverTree dp -> Bool
property_covering Unit = True
property_covering (UnitLift node) = if Map.size (childrenMap node) > 0 
    then maximum (map (distance (nodedp node) . nodedp) $ Map.elems $ childrenMap node) < coverDist node 
      && and (map (property_covering . UnitLift) $ Map.elems $ childrenMap node)
    else True

property_leveled  :: MetricSpace dp => CoverTree dp -> Bool
property_leveled (Unit) = True
property_leveled (UnitLift node) = case map sepdist (Map.elems $ childrenMap node) of
    [] -> True
    xs -> all (== head xs) xs
       && and (map (property_leveled . UnitLift) $ Map.elems $ childrenMap node)

property_separating  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_separating Unit = True
property_separating (UnitLift node) = if Map.size (childrenMap node) > 1
    then foldl1' min ((mapFactorial stMaxDistance) $ Map.elems $ childrenMap node) > sepdist_child node
      && and (map (property_separating . UnitLift) $ Map.elems $ childrenMap node)
    else True
    where
        mapFactorial :: (a -> a -> b) -> [a] -> [b]
        mapFactorial f xs = go xs []
            where
                go [] ys = ys
                go (x:xs) ys = go xs (map (f x) xs ++ ys)

property_maxDescendentDistance  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) = if stIsLeaf node
    then True
    else and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
      && and (map (\dp -> weight node == 1 
                       || distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

property_validmerge :: CoverTree (Double,Double) -> CoverTree (Double,Double) -> Bool
property_validmerge (UnitLift ct1) (UnitLift ct2) = property_leveled . UnitLift $ ct1 `unsafeMerge` ct2

property_lossless :: [(Double,Double)] ->  Bool
property_lossless [] = True
property_lossless xs = Set.fromList xs == dpSet ct
    where
        UnitLift ct = train xs :: AddUnit (CoverTree' (2/1)) () (Double,Double)

        dpSet :: (Ord dp) => CoverTree' base tag dp -> Set.Set dp
        dpSet = Set.fromList . dpList
            where
                dpList :: CoverTree' base tag dp -> [dp]
                dpList node = nodedp node:(concat . map dpList . Map.elems $ childrenMap node)

property_numdp :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
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

ys :: [(Double,Double)]
-- ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
-- ys = [(0,0),(0,10),(10,10),(10,0),(10,-10),(0,-10),(-10,-10),(-10,0),(-10,10)]
ys = [(0,0),(0,10),(8,8),(10,0),(8,-8),(0,-10),(-8,-8),(-10,0),(-8,8)]
my = train ys :: CoverTree (Double,Double)
my2 = train $ take 3 ys :: CoverTree (Double,Double)
-- my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
my' = train ys' :: CoverTree (Double,Double)
-- my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
-- zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(20,10),(22,21)]
mz = train zs :: CoverTree (Double,Double)
-- mz = prunect $ insertBatch zs

mq = mz `mappend` my

grid :: Int -> Int -> [(Double,Double)]
grid n 2 = take n [(x,y) | x <- [1..w], y <- [1..w]]
    where
        n' = fromIntegral n
        w = fromIntegral $ ceiling $ n' ** (1/2)

gs = grid 9 2
gs' i = take i gs
mg = train gs :: CoverTree (Double,Double)
mg' i = train $ gs' i :: CoverTree (Double,Double)

mg3 = train1dp (1,1) `mappend` ( train1dp (1,2) `mappend` train1dp (1,3) ) :: CoverTree (Double,Double)

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
                   Map.foldr (|||) mempty $ Map.map (draw' (depth+1)) $ childrenMap tree)
                
    where
        label = intShow $ nodedp tree
        nodecolor = if weight tree > 0
            then red
            else lightblue

        mkConnections = connect (label++show depth) (label++show (depth+1)) 
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (Map.keys $ childrenMap tree))
            
justdouble :: Maybe Double -> String
justdouble Nothing = "0"
justdouble (Just x) = show x

apList :: [a -> a] -> a -> a
apList [] a = a
apList (x:xs) a = apList xs (x a)

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
