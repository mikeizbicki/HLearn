{-# LANGUAGE NoMonomorphismRestriction,DataKinds #-}

{-# LANGUAGE BangPatterns, FlexibleContexts,FlexibleInstances,UndecidableInstances,TypeFamilies,ScopedTypeVariables #-}

module HLearn.DataStructures.CoverTree
    where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.List hiding (insert)
import Data.Maybe
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
    , children'  :: !(Map.Map dp (CoverTree' tag dp)) 
    , tag        :: !(Maybe tag)
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
    {-# INLINABLE stChildren #-}
    {-# INLINABLE stNode #-}
    {-# INLINABLE stHasNode #-}
    {-# INLINABLE stIsLeaf #-}

    stMinDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2) - (coverDist ct1) - (coverDist ct2) 
    stMaxDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2) + (coverDist ct1) + (coverDist ct2) 

    stMinDistanceDpWithDistance ct dp = let dist = distance (nodedp ct) dp in (dist - coverDist ct, dist)
    stMaxDistanceDpWithDistance ct dp = let dist = distance (nodedp ct) dp in (dist + coverDist ct, dist)

    stChildren = Map.elems . children
    stNode = nodedp
    stHasNode _ = True
    stIsLeaf ct = Map.size (children' ct) == 0

instance Taggable CoverTree' where
    {-# INLINABLE getTag #-}
    {-# INLINABLE setTag #-}
    {-# INLINABLE mapTag #-}
    
    getTag = tag
    setTag tag ct = ct { tag = Just tag } 

    mapTag f ct = ct
        { children' = Map.map (mapTag f) $ children' ct
        , tag       = Just $ f $ tag ct
        }

---------------------------------------

isSingleton :: CoverTree' tag dp -> Bool
isSingleton node = Map.size (children' node) == 0

coverDist :: (Fractional (Ring dp)) => CoverTree' tag dp -> Ring dp
coverDist node = sepdist node*coverfactor

{-# INLINE coverfactor #-}
coverfactor = 2 --1.3

cover_knn2' (UnitLift x) (UnitLift y) = cover_knn2 x [y]

cover_knn2 :: forall k tag dp. 
    ( MetricSpace dp
    , Ord dp
    ) => CoverTree' tag dp -> [CoverTree' tag dp] -> KNN2 2 dp 
cover_knn2 !q !rs = if and $ map stIsLeaf rs
    then KNN2 $ Map.fromList $ map mkKNN $ stDescendents q
    else if sepdist q < sepdist (head rs)
        then cover_knn2 q rs_children 
        else reduce $ map (\q' -> cover_knn2 q' rs) $ stChildren q
    where
        rs_children = concatMap stChildren rs
        minchild = minimum $ map (\r -> Neighbor (stNode r) (distance (stNode q) (stNode r))) rs
        rs_children' = filter (\r -> stMaxDistance q r <= neighborDistance minchild) rs_children

        mkKNN !dp = (dp, KNN [minimum $ map mkNeighbor rs])
            where
                mkNeighbor r' = Neighbor (stNode r') $ distance (stNode r') dp

---------------------------------------
-- insertion as described in the paper

safeInsert :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp
    , Floating (Ring dp)
    ) => CoverTree' tag dp -> dp -> CoverTree' tag dp
safeInsert !node !dp = case insert node dp of
    Just x -> x
    Nothing -> Node
        { nodedp    = nodedp node
        , sepdist   = dist2up dist
        , tag       = Nothing
        , children' = Map.fromList 
            [ (nodedp node, growct node (dist2down dist))
            , (dp, Node dp (dist2down dist) mempty Nothing)
            ]
        }
        where
            dist = distance (nodedp node) dp


insert :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp, Fractional (Ring dp)
    ) => CoverTree' tag dp -> dp -> Maybe (CoverTree' tag dp)
insert !node !dp = if isFartherThan dp (nodedp node) (sepdist node)
    then Nothing
    else seq justnode $ Just justnode
    where 
        justnode = node
            { children' = if hasInsert
                then Map.insert key val $ children node
                else Map.insert dp (Node dp (sepdist node/coverfactor) mempty Nothing) $ children node
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
    ) => [dp] -> CoverTree' tag dp
insertBatch ((!x):xs) = go xs $ Node x 0 mempty Nothing
    where
        go [] tree = tree
        go (x:xs) tree = go xs $ safeInsert tree x

-------------------------------------------------------------------------------
-- algebra

instance F.Foldable (CoverTree' tag) where
    foldr f i ct = if Map.size (children' ct) == 0
        then f (nodedp ct) i
        else foldr (\ct' i' -> F.foldr f i' ct') i (Map.elems $ children' ct)

instance (HasRing dp) => HasRing (CoverTree' tag dp) where
    type Ring (CoverTree' tag dp) = Ring dp

instance
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    , Show (Ring dp)
    , Show dp
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

merge' :: (Ord dp, MetricSpace dp) => CoverTree' tag dp -> CoverTree' tag dp -> Maybe (CoverTree' tag dp, [CoverTree' tag dp])
-- merge' !ct1 !ct2 = if distance (nodedp ct1) (nodedp ct2) > (sepdist ct1)
merge' !ct1 !ct2 = if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Nothing
    else Just ( ct1 { children' = newchildren' `Map.union` Map.fromList (map (\x -> (nodedp x,growct x (sepdist ct1/coverfactor))) valid_newleftovers) }
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
children tree = Map.insertWith 
    (\x y -> y) 
    (nodedp tree) 
    (Node 
        { nodedp    = nodedp tree
        , sepdist   = sepdist tree/coverfactor
        , children' = mempty
        , tag       = Nothing
        })
    (children' tree)

prunect :: CoverTree' tag dp -> CoverTree' tag dp
prunect ct = if Map.size (children' ct) == 1 
    then head $ Map.elems $ children' ct
    else ct

growct :: (Fractional (Ring dp),Ord (Ring dp)) => CoverTree' tag dp -> Ring dp -> CoverTree' tag dp
growct ct d = if sepdist ct==0 || Map.size (children' ct)==0
    then ct { sepdist=d }
    else if d > sepdist ct
        then growct (Node
            { nodedp=nodedp ct
            , sepdist=sepdist ct*coverfactor
            , children' = Map.singleton (nodedp ct) ct 
            , tag = Nothing
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
    , Show dp
    , Show (Ring dp)
    ) => HomTrainer (CoverTree dp) 
        where
    type Datapoint (CoverTree dp) = dp
    {-# INLINE train1dp #-}
    train1dp dp = UnitLift $ Node dp 0 mempty Nothing

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
    where
--         f = trace ("mapFact="++show ((mapFactorial ctMaxDistance) $ Map.elems $ children' node)++", sepdist="++show (sepdist node)++ "nodes="++show (Map.keys $ children' node))

mapFactorial :: (a -> a -> b) -> [a] -> [b]
mapFactorial f [] = []
mapFactorial f (x:xs) = map (f x) xs ++ mapFactorial f xs

property_lossless :: [(Double,Double)] ->  Bool
property_lossless [] = True
property_lossless xs = Set.fromList xs == dpSet ct
    where
        UnitLift ct = train xs :: CoverTree (Double,Double)

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
                         === (text (show(sepdist tree)) <> strutY 0.5)) 
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
