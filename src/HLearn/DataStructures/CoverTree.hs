{-# LANGUAGE NoMonomorphismRestriction,DataKinds #-}

{-# LANGUAGE BangPatterns, FlexibleContexts,FlexibleInstances,UndecidableInstances,TypeFamilies,ScopedTypeVariables #-}

module HLearn.DataStructures.CoverTree
    ( CoverTree
    , CoverTree'

    -- * Pruning
    , pruneExtraLeaves
    , pruneSingletons

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
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Test.QuickCheck
import Debug.Trace

import Diagrams.Prelude hiding (distance,trace,query)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>),(|>))
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = AddUnit CoverTree' () dp

data CoverTree' tag dp = Node 
    { nodedp     :: !dp
    , sepdist    :: !(Ring dp)
    , maxDescendentDistance :: (Ring dp)
    , children'  :: !(Map.Map dp (CoverTree' tag dp)) 
    , tag        :: !tag
    }

deriving instance (Read (Ring dp), Read tag, Read dp, Ord dp) => Read (CoverTree' tag dp)
deriving instance (Show (Ring dp), Show tag, Show dp, Ord dp) => Show (CoverTree' tag dp)

instance (Eq dp, Eq (Ring dp)) => Eq (CoverTree' tag dp) where
    ct1 == ct2 = sepdist ct1 == sepdist ct2 && nodedp ct1 == nodedp ct2

instance (Ord dp, Ord (Ring dp)) => Ord (CoverTree' tag dp) where
    compare ct1 ct2 = compare (sepdist ct1) (sepdist ct2)
                   <> compare (nodedp ct1) (nodedp ct2)

instance (NFData dp,NFData (Ring dp),NFData tag) => NFData (CoverTree' tag dp) where
    rnf ct = deepseq (nodedp ct) 
           $ deepseq (sepdist ct)
           $ deepseq (children' ct)
           $ deepseq (tag ct)
           $ ()

instance 
    ( HasRing dp
    , MetricSpace dp
    , Ring dp ~ Ring (CoverTree' tag dp)
    , Ord dp
    ) => SpaceTree (CoverTree' tag) dp
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

    stMinDistanceWithDistance !ct1 !ct2 = (dist - (coverDist ct1) - (coverDist ct2), dist)
        where dist = distance (nodedp ct1) (nodedp ct2) 
    stMaxDistanceWithDistance !ct1 !ct2 = (dist + (coverDist ct1) + (coverDist ct2), dist)
        where dist = distance (nodedp ct1) (nodedp ct2) 

--     stMinDistanceDpWithDistance !ct !dp = let dist = distance (nodedp ct) dp in (dist - coverDist ct, dist)
--     stMaxDistanceDpWithDistance !ct !dp = let dist = distance (nodedp ct) dp in (dist + coverDist ct, dist)

--     stMinDistanceDpFromDistance !ct !dp !dist = dist - coverDist ct
--     stMaxDistanceDpFromDistance !ct !dp !dist = dist + coverDist ct

--     stMinDistanceWithDistance !ct1 !ct2 = (dist - (maxDescendentDistance ct1) - (maxDescendentDistance ct2), dist)
--         where dist = distance (nodedp ct1) (nodedp ct2) 
--     stMaxDistanceWithDistance !ct1 !ct2 = (dist + (maxDescendentDistance ct1) + (maxDescendentDistance ct2), dist)
--         where dist = distance (nodedp ct1) (nodedp ct2) 
 

    stMinDistanceDpWithDistance !ct !dp = let dist = distance (nodedp ct) dp in (dist - maxDescendentDistance ct, dist)
    stMaxDistanceDpWithDistance !ct !dp = let dist = distance (nodedp ct) dp in (dist + maxDescendentDistance ct, dist)

    stMinDistanceDpFromDistance !ct !dp !dist = dist - maxDescendentDistance ct
    stMaxDistanceDpFromDistance !ct !dp !dist = dist + maxDescendentDistance ct

    stChildren = Map.elems . children
    stNode = nodedp
    stHasNode _ = True
    stIsLeaf ct = Map.size (children' ct) == 0

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 
--     lambda !ct = sepdist ct * coverfactor

instance Ord dp => Taggable CoverTree' dp where
    {-# INLINABLE getTag #-}
    {-# INLINABLE initTags #-}
    {-# INLINABLE clearTags #-}
    
    getTag = tag

--     initTags ct = (\(x,_,_) -> x)  $ initTags' ct (nodedp ct) 0 1 0
    initTags ct = let (a,(_,b),c) = initTags' ct (nodedp ct) 0 1 0 in (a,c+1,b)
        where
            initTags' ct parentdp parenttag dptag nodetag = (ct
                { tag = (nodetag, snd cttag)
                , children' = new_children
                }
                ,new_dptag
                ,new_nodetag
                )
                where
                    cttag = if nodedp ct == parentdp then parenttag else dptag
                    dptag' = if nodedp ct == parentdp then dptag else dptag+1

                    go [] (new_children,new_dptag,new_nodetag) 
                        = (new_children,new_dptag,new_nodetag)
                    go (x:xs) (new_children,new_dptag,new_nodetag) 
                        = go xs (new_children',new_dptag',new_nodetag')
                        where
                            new_children' = Map.insert (nodedp x') x' new_children
                            (x',new_dptag',new_nodetag') = initTags' x (nodedp ct) cttag new_dptag (new_nodetag+1)
                    
                    (new_children,new_dptag,new_nodetag) 
                        = go (Map.elems $ children' ct) (Map.empty,dptag',nodetag)

    
    clearTags ct = ct
        { tag = ()
        , children' = Map.map clearTags $ children' ct
        }


---------------------------------------

isSingleton :: CoverTree' tag dp -> Bool
isSingleton node = Map.size (children' node) == 0

coverDist :: (Fractional (Ring dp)) => CoverTree' tag dp -> Ring dp
coverDist node = sepdist node*coverfactor

coverfactor = 2
-- coverfactor = 1.5
-- coverfactor = 1.3

{-# INLINABLE pruneExtraLeaves #-}
pruneExtraLeaves :: (MetricSpace dp, Ord dp) => CoverTree' tag dp -> CoverTree' tag dp
pruneExtraLeaves ct = if stIsLeaf ct
    then ct
    else ct { children' = Map.filter (\c -> not $ stNode c==stNode ct && stIsLeaf c) 
                        $ Map.map pruneExtraLeaves $ children' ct }

{-# INLINABLE pruneSingletons #-}
pruneSingletons :: (MetricSpace dp,Ord dp) => CoverTree' tag dp -> CoverTree' tag dp
pruneSingletons ct = if stIsLeaf ct
    then ct
    else ct { children' = newchildren }
    where
        c = head $ F.toList $ children' ct
        newchildren = if Map.size (children' ct) == 1 && Map.size (children' c) == 1 
            then children' $ pruneSingletons c
            else Map.map pruneSingletons $ children' ct

---------------------------------------
-- insertion as described in the paper

safeInsert :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Floating (Ring dp)
    , Monoid tag
    ) => CoverTree' tag dp -> dp -> CoverTree' tag dp
safeInsert !node !dp = case insert node dp of
    Just x -> x
    Nothing -> Node
        { nodedp    = nodedp node
        , sepdist   = dist2up dist
        , tag       = mempty
        , children' = Map.fromList 
            [ (nodedp node, growct node (dist2down dist))
            , (dp, Node 
                { nodedp    = dp 
                , sepdist   = dist2down dist
                , tag       = mempty
                , children' = mempty
                , maxDescendentDistance = 0
                })
            ]
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }
        where
            dist = distance (nodedp node) dp


insert :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Fractional (Ring dp)
    , Monoid tag
    ) => CoverTree' tag dp -> dp -> Maybe (CoverTree' tag dp)
insert !node !dp = if isFartherThan dp (nodedp node) (sepdist node)
    then Nothing
    else seq justnode $ Just justnode
    where 
        justnode = Node
            { nodedp = nodedp node
            , sepdist = sepdist node
            , tag = tag node
            , maxDescendentDistance = max
                (maxDescendentDistance node)
                (distance (nodedp node) dp)
            , children' = if hasInsert
                then Map.insert key val $ children node
                else Map.insert dp (Node dp (sepdist node/coverfactor) 0 mempty mempty) $ children node
            }

        viableChildren = Map.filter (\subtree -> not $ isFartherThan (nodedp subtree) dp (coverDist subtree)) $ children node
        childrenInserts = Map.map (\tree -> insert tree dp) viableChildren

        insertables = Map.filterWithKey filtergo childrenInserts
            where
                filtergo _ Nothing   = False
                filtergo _ (Just _)  = True

        (key,Just val):xs = {-sortBy sortgo $-} Map.assocs insertables
            where
                sortgo (_,Just v1) (_,Just v2) = compare (Map.size $ children' v1) (Map.size $ children' v2)

        hasInsert = Map.size insertables > 0

insertBatch :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Floating (Ring dp)
    , Monoid tag
    ) => [dp] -> CoverTree' tag dp
insertBatch ((!x):xs) = go xs $ Node x 0 0 mempty mempty
    where
        go [] tree = tree
        go (x:xs) tree = go xs $ safeInsert tree x

-------------------------------------------------------------------------------
-- algebra

-- instance F.Foldable (CoverTree' tag) where
--     foldr f i ct = if Map.size (children' ct) == 0
--         then f (nodedp ct) i
--         else foldr (\ct' i' -> F.foldr f i' ct') i' (Map.elems $ children' ct)
--         where
--             i' = if nodedp ct `Map.member` children' ct
--                 then i
--                 else f (nodedp ct) i

instance (HasRing dp) => HasRing (CoverTree' tag dp) where
    type Ring (CoverTree' tag dp) = Ring dp

instance
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    , Monoid tag
--     , Show (Ring dp)
--     , Show dp
    ) => Semigroup (CoverTree' tag dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = merge ct1 ct2 

merge ct1 ct2 = case merge' ct1' ct2'  of
    Just (ct,[]) -> ct
--     Just (ct,xs) -> foldr merge ct xs
    Just (ct,xs) -> foldl' merge ct xs
    Nothing -> merge (growct ct1' (maxlevel*coverfactor)) ct2'
    where
        ct1' = growct ct1 maxlevel
        ct2' = growct ct2 maxlevel
        maxlevel = maximum [(sepdist ct1), (sepdist ct2),1]

merge' :: (Ord dp, MetricSpace dp, Monoid tag) => CoverTree' tag dp -> CoverTree' tag dp -> Maybe (CoverTree' tag dp, [CoverTree' tag dp])
-- merge' !ct1 !ct2 = if distance (nodedp ct1) (nodedp ct2) > (sepdist ct1)
merge' !ct1 !ct2 = if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Nothing
    else Just ( ct1 
        { children' = newchildren' `Map.union` Map.fromList (map (\x -> (nodedp x,growct x (sepdist ct1/coverfactor))) valid_newleftovers) 
        , maxDescendentDistance = maximum $ map (distance (nodedp ct1)) $ (stDescendents ct2++stDescendents ct1)
        }
        , invalid_newleftovers++invalidchildren
        )
        
    where
--         validchild x = distance (nodedp ct1) (nodedp x) <= sepdist ct1
        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (sepdist ct1)
        validchildren = filter validchild $ Map.elems $ children ct2
        invalidchildren = filter (not . validchild) $ Map.elems $ children ct2

        (newchildren',newleftovers) = go (children ct1,[]) validchildren
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

children :: (Ord dp,Fractional (Ring dp)) => CoverTree' tag dp -> Map.Map dp (CoverTree' tag dp)
children tree = if Map.size (children' tree) > 0
    then children' tree
    else Map.singleton (nodedp tree) $ Node
        { nodedp    = nodedp tree
        , sepdist   = sepdist tree/coverfactor
        , tag       = tag tree
        , children' = mempty
        , maxDescendentDistance = 0
        }
    
--     Map.insertWith 
--     (\x y -> y) 
--     (nodedp tree) 
--     (Node 
--         { nodedp    = nodedp tree
--         , sepdist   = sepdist tree/coverfactor
--         , children' = mempty
--         , tag       = tag tree
--         })
--     (children' tree)

prunect :: CoverTree' tag dp -> CoverTree' tag dp
prunect ct = if Map.size (children' ct) == 1 
    then head $ Map.elems $ children' ct
    else ct

growct :: (Fractional (Ring dp),Ord (Ring dp),Monoid tag) => CoverTree' tag dp -> Ring dp -> CoverTree' tag dp
growct ct d = if sepdist ct==0 || Map.size (children' ct)==0
    then ct { sepdist=d }
    else if d > sepdist ct
        then growct (Node
            { nodedp=nodedp ct
            , sepdist=sepdist ct*coverfactor
            , children' = Map.singleton (nodedp ct) ct 
            , maxDescendentDistance = maxDescendentDistance ct
            , tag = mempty
            }
            ) d
        else ct

dist2up :: (Floating d,RealFrac d) => d -> d
dist2up d = dist2down d * coverfactor

dist2down :: (Floating d,RealFrac d) => d -> d
dist2down d = coverfactor^^(floor $ log d / log coverfactor :: Int)

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
    train1dp dp = UnitLift $ Node dp 0 0 mempty mempty 

    {-# INLINABLE train #-}
    train = UnitLift . insertBatch . F.toList

train_insertBatch = UnitLift . insertBatch . F.toList

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
        return $ train xs 

property_covering :: MetricSpace dp => CoverTree dp -> Bool
property_covering Unit = True
property_covering (UnitLift node) = if Map.size (children' node) > 0 
    then maximum (map (distance (nodedp node) . nodedp) $ Map.elems $ children' node) < coverDist node 
      && and (map (property_covering . UnitLift) $ Map.elems $ children' node)
    else True

property_leveled  :: MetricSpace dp => CoverTree dp -> Bool
property_leveled (Unit) = True
property_leveled (UnitLift node) = case map sepdist (Map.elems $ children' node) of
    [] -> True
    xs -> all (== head xs) xs
       && and (map (property_leveled . UnitLift) $ Map.elems $ children' node)

property_separating  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_separating Unit = True
property_separating (UnitLift node) = if Map.size (children' node) > 1
    then minimum ((mapFactorial stMaxDistance) $ Map.elems $ children' node) > (sepdist $ head $ Map.elems $ children' node)
      && and (map (property_separating . UnitLift) $ Map.elems $ children' node)
    else True

property_maxDescendentDistance  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) = if stIsLeaf node
    then True
    else and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
      && and (map (\dp -> distance dp (nodedp node) <= (maxDescendentDistance node)) $ stDescendents node)
--       && and (map (\dp -> not $ isFartherThan dp (nodedp node) (maxDescendentDistance node)) $ stDescendents node)

mapFactorial :: (a -> a -> b) -> [a] -> [b]
mapFactorial f [] = []
mapFactorial f (x:xs) = map (f x) xs ++ mapFactorial f xs

property_lossless :: [(Double,Double)] ->  Bool
property_lossless [] = True
property_lossless xs = Set.fromList xs == dpSet ct
    where
        UnitLift ct = train xs :: AddUnit CoverTree' () (Double,Double)

dpSet :: (Ord dp) => CoverTree' tag dp -> Set.Set dp
dpSet = Set.fromList . dpList
    where
        dpList :: CoverTree' tag dp -> [dp]
        dpList node = nodedp node:(concat . map dpList . Map.elems $ children' node)

---------------------------------------

randL :: Int -> IO [(Double,Double)]
randL n = replicateM n $ do
    x <- randomRIO (-100,100)
    y <- randomRIO (-100,100)
    return (fromIntegral (x :: Int), fromIntegral (y :: Int))

ys :: [(Double,Double)]
ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
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

draw (UnitLift node) = draw' 0 node
draw' depth tree = mkConnections $ 
                   (named (label++show depth) $ fontSize 0.5 $ 
                        (
                             (text label <> strutY 1) 
                         === (text (show (sepdist tree)) <> strutY 0.5) 
                         === (text (show (maxDescendentDistance tree)) <> strutY 0.5)
                        ) 
                          <> circle 1 # fc red) 
               === (pad 1.05 $ centerName (label++show (depth+1)) $ 
                   Map.foldr (|||) mempty $ Map.map (draw' (depth+1)) $ children' tree)
                
    where
        label = intShow $ nodedp tree

        mkConnections = connect (label++show depth) (label++show (depth+1)) 
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (Map.keys $ children' tree))
            
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

intShow :: (Double,Double) -> String
intShow (x,y) = show (floor x::Int,floor y::Int)
