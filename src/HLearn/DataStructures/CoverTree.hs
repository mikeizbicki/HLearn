{-# LANGUAGE NoMonomorphismRestriction,DataKinds,ScopedTypeVariables,KindSignatures #-}

{-# LANGUAGE BangPatterns, FlexibleContexts,FlexibleInstances,UndecidableInstances,TypeFamilies,ScopedTypeVariables #-}

module HLearn.DataStructures.CoverTree
    ( CoverTree
    , CoverTree'

    -- * Pruning
    , pruneExtraLeaves
    , pruneSingletons
    , addGhostData

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

import HLearn.Algebra hiding ((#),(<>),(|>))
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
--         case isFartherThanWithDistance (nodedp ct) dp (b-maxDescendentDistance ct) of
        case isFartherThanWithDistance (nodedp ct) dp (b+maxDescendentDistance ct) of
--         case isFartherThanWithDistance (nodedp ct) dp b of
            Strict.Nothing -> Strict.Nothing
            Strict.Just dist -> Strict.Just $ dist

    stIsMaxDistanceDpFartherThanWithDistance !ct !dp !b = 
        isFartherThanWithDistance (nodedp ct) dp (b-maxDescendentDistance ct)

    stMinDistanceDpFromDistance !ct !dp !dist = dist-maxDescendentDistance ct
    stMaxDistanceDpFromDistance !ct !dp !dist = dist+maxDescendentDistance ct

--     stChildren  = Map.elems . childrenMap
    stChildren  = {-# SCC stChildren_1 #-} childrenList
    stNode      = nodedp
    stWeight    = weight
    stHasNode _ = True
    stIsLeaf ct = Map.size (childrenMap ct) == 0

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 

-- instance Ord dp => Taggable (CoverTree' base) dp where
--     {-# INLINABLE getTag #-}
--     {-# INLINABLE initTags #-}
--     {-# INLINABLE clearTags #-}
--     
--     getTag = tag
-- 
--     initTags ct = let (a,(_,b),c) = initTags' ct (nodedp ct) 0 1 0 in (a,c+1,b)
--         where
--             initTags' ct parentdp parenttag dptag nodetag = (ct
--                 { tag = (nodetag, snd cttag)
--                 , childrenMap = new_children
--                 }
--                 ,new_dptag
--                 ,new_nodetag
--                 )
--                 where
--                     cttag = if nodedp ct == parentdp then parenttag else dptag
--                     dptag' = if nodedp ct == parentdp then dptag else dptag+1
-- 
--                     go [] (new_children,new_dptag,new_nodetag) 
--                         = (new_children,new_dptag,new_nodetag)
--                     go (x:xs) (new_children,new_dptag,new_nodetag) 
--                         = go xs (new_childrenMap,new_dptag',new_nodetag')
--                         where
--                             new_childrenMap = Map.insert (nodedp x') x' new_children
--                             (x',new_dptag',new_nodetag') = initTags' x (nodedp ct) cttag new_dptag (new_nodetag+1)
--                     
--                     (new_children,new_dptag,new_nodetag) 
--                         = go (Map.elems $ childrenMap ct) (Map.empty,dptag',nodetag)
-- 
--     
--     clearTags ct = ct
--         { tag = ()
--         , childrenMap = Map.map clearTags $ childrenMap ct
--         }


---------------------------------------

isSingleton :: CoverTree' base tag dp -> Bool
isSingleton node = Map.size (childrenMap node) == 0

coverDist :: forall base tag dp.
    ( Fractional (Ring dp)
    , SingI base
    ) => CoverTree' base tag dp -> Ring dp
coverDist node = sepdist node*coverfactor 
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINABLE pruneExtraLeaves #-}
pruneExtraLeaves :: forall base tag dp.
     ( MetricSpace dp
     , Ord dp
     , SingI base
     ) => CoverTree' base tag dp -> CoverTree' base tag dp
pruneExtraLeaves ct = if stIsLeaf ct
    then ct
    else ct
        { childrenMap = childrenmap'
        , childrenList = Map.elems childrenmap'
        }

    where
        childrenmap' = Map.filter (\c -> not $ stNode c==stNode ct && stIsLeaf c) 
                     $ Map.map pruneExtraLeaves $ childrenMap ct

{-# INLINABLE pruneSingletons #-}
pruneSingletons :: forall base tag dp.
    ( MetricSpace dp
    , Ord dp
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp
pruneSingletons ct = if stIsLeaf ct
    then ct
    else ct { childrenMap = newchildren }
    where
        c = head $ F.toList $ childrenMap ct
        newchildren = if Map.size (childrenMap ct) == 1 && Map.size (childrenMap c) == 1 
            then childrenMap $ pruneSingletons c
            else Map.map pruneSingletons $ childrenMap ct

{-# INLINABLE addGhostData #-}
addGhostData :: forall base tag dp.
    ( MkCentroid dp
    , Ord dp
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp
addGhostData !ct = ct 
    { childrenMap = childrenmap'
    , childrenList = Map.elems childrenmap'    
    }
    where
        childrenmap' = Map.map addGhostData $ if Map.size (childrenMap ct) < 10
            then childrenMap ct
            else childrenMap $ go (stChildren ct) ct
        
        check ct1 ct2 = -- trace ("check "++show (nodedp ct1)++" "++show (nodedp ct2)) $
          if stIsLeaf ct1
             && stIsLeaf ct2
             && not (isFartherThan dp1 c $ child_sepdist ct) && not (isFartherThan dp2 c $ child_sepdist ct)
            then Just c
            else Nothing
            where 
                dp1 = nodedp ct1
                dp2 = nodedp ct2
                c = mkCentroid dp1 dp2

        go [] !ct = ct 
        go (x:xs) !ct = if not $ stIsLeaf x 
            then go xs ct
            else case filter isJust $ map (check x) xs of
                [] -> go xs ct
                ((Just ghostdp):ys) -> if length eligibleChildren<5
                    then go xs ct
                    else go fail $ ct 
                        { childrenMap 
                            = Map.insert ghostdp (Node
                                { nodedp    = ghostdp
                                , sepdist   = child_sepdist ct
                                , weight    = 0
                                , tag       = mempty
                                , childrenMap = childrenMapVal
                                , childrenList = Map.elems childrenMapVal
    --                             , maxDescendentDistance = child_sepdist ct 
                                , maxDescendentDistance = 
                                    maximum $ map (distance ghostdp . nodedp) $ x:eligibleChildren
                                })
                            $ Map.delete (nodedp x)
                            $ foldl' (\map ct -> Map.delete (nodedp ct) map) (childrenMap ct) eligibleChildren
                        } 
                    where
                        childrenMapVal = Map.insert (nodedp x) ( x { sepdist = child_sepdist x })
                                       $ Map.fromList $ zip (map nodedp $ eligibleChildren) eligibleChildren 


                        eligibleChildren = map (\ct -> ct { sepdist = child_sepdist ct }) pass

                        (pass,fail) = partition 
                            (\child -> stIsLeaf child 
                                    && (not $ isFartherThan (nodedp child) ghostdp (child_sepdist ct))) 
                            $ xs -- stChildren ct

child_sepdist :: forall base tag dp.
    ( SingI base
    , MetricSpace dp
    ) => CoverTree' base tag dp -> Ring dp
child_sepdist ct = sepdist ct/coverfactor
    where
        coverfactor = fromSing (sing :: Sing base) 

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
        , sepdist   = dist2up (sing::Sing base) dist
        , weight    = 0 -- weight node
        , tag       = mempty
        , childrenMap = childrenMapVal
        , childrenList = Map.elems childrenMapVal
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }
        where
            childrenMapVal = Map.fromList 
                [ (nodedp node, growct node (dist2down (sing::Sing base) dist))
                , (dp, Node 
                    { nodedp    = dp 
                    , weight    = w
                    , sepdist   = dist2down (sing::Sing base) dist
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
            , tag       = tag node
            , childrenMap = childrenMapVal
            , childrenList = Map.elems childrenMapVal
            , maxDescendentDistance = max
                (maxDescendentDistance node)
                (distance (nodedp node) dp)
            }

        childrenMapVal = if hasInsert
            then Map.insert key val $ childrenMap node
            else Map.insert dp (Node 
                { nodedp  = dp 
                , sepdist = sepdist node/coverfactor
                , weight  = w 
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
    , Fractional (Ring dp)
    , Monoid tag
    , SingI base
    ) => Semigroup (CoverTree' base tag dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = merge ct1 ct2 

merge :: forall base tag dp.
    ( Ord dp
    , MetricSpace dp
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp -> CoverTree' base tag dp
merge ct1 ct2 = case merge' ct1' ct2'  of
    Just (ct,[]) -> ct
    Just (ct,xs) -> foldl' merge ct xs
    Nothing -> merge (growct ct1' (maxlevel*coverfactor)) ct2'
    where
        ct1' = growct ct1 maxlevel
        ct2' = growct ct2 maxlevel
        maxlevel = maximum [(sepdist ct1), (sepdist ct2),1]
        coverfactor = fromSing (sing :: Sing base)

merge' :: forall base tag dp.
    ( Ord dp
    , MetricSpace dp
    , Monoid tag
    , SingI base
    ) => CoverTree' base tag dp -> CoverTree' base tag dp -> Maybe (CoverTree' base tag dp, [CoverTree' base tag dp])
merge' !ct1 !ct2 = if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Nothing
    else Just ( ct1 
        { childrenMap = newchildrenMap `Map.union` Map.fromList (map (\x -> (nodedp x,growct x (sepdist ct1/coverfactor))) valid_newleftovers) 
        , maxDescendentDistance = maximum $ map (distance (nodedp ct1)) $ (stDescendents ct2++stDescendents ct1)
        }
        , invalid_newleftovers++invalidchildren
        )
        
    where
        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (sepdist ct1)
        validchildren = filter validchild $ Map.elems $ childrenMap ct2
        invalidchildren = filter (not . validchild) $ Map.elems $ childrenMap ct2

        (newchildrenMap,newleftovers) = go (childrenMap ct1,[]) validchildren
        valid_newleftovers = filter validchild newleftovers
        invalid_newleftovers = filter (not.validchild) newleftovers

        go (!childmap,leftovers) ![] = (childmap,leftovers)
        go (!childmap,leftovers) !(x:xs) = 
            case filter (isJust.snd) $ map (\(k,v)->(k,merge' v x)) $ Map.assocs childmap of
                [] -> 
                    go ( Map.insert (nodedp x) (x {sepdist=sepdist ct1/coverfactor}) childmap
                       , leftovers
                       ) xs
                (old,Just (new,leftovers')):ys -> 
                    go ( Map.insert (nodedp new) (new {sepdist=sepdist ct1/coverfactor}) 
                          $ Map.delete old childmap
                       , leftovers'++leftovers
                       ) xs

        coverfactor = fromSing (sing :: Sing base)

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
            , tag       = mempty
            , childrenMap = Map.singleton (nodedp ct) ct 
            , childrenList = [ct]
            , maxDescendentDistance = maxDescendentDistance ct
            }
            ) d
        else ct
    where
        coverfactor = fromSing (sing :: Sing base)


{-# INLINE dist2up #-}
dist2up :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
dist2up s d = dist2down s d * coverfactor
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINE dist2down #-}
dist2down :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
dist2down _ d = coverfactor^^(floor $ log d / log coverfactor :: Int)
    where
        coverfactor = fromSing (sing :: Sing base)

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
--             trace ("(x,y)="++show (x,y)) $ return (x,y)
            return (x,y)
        return $ UnitLift $ addGhostData . unUnit $ train xs 

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
    then foldl1' min ((mapFactorial stMaxDistance) $ Map.elems $ childrenMap node) > child_sepdist node
      && and (map (property_separating . UnitLift) $ Map.elems $ childrenMap node)
    else True

property_maxDescendentDistance  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) = if stIsLeaf node
    then True
    else and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
      && and (map (\dp -> weight node == 1 
                       || distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

mapFactorial :: (a -> a -> b) -> [a] -> [b]
mapFactorial f xs = go xs []
    where
        go [] ys = ys
        go (x:xs) ys = go xs (map (f x) xs ++ ys)

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
