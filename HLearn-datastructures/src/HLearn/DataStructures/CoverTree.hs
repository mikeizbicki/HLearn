{-# LANGUAGE NoMonomorphismRestriction #-}

module HLearn.DataStructures.CoverTree
    where

import Control.Monad
import Control.Monad.Random
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Test.QuickCheck
import Debug.Trace

import Diagrams.Prelude hiding (distance,trace)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>))

-------------------------------------------------------------------------------
-- data types

data CoverTree dp 
    = Leaf
    | Node 
        { nodedp    :: !dp
        , level     :: !Int
        , children' :: !(Map.Map dp (CoverTree dp)) 
        }
    deriving (Read,Show,Eq,Ord)

isSingleton :: CoverTree dp -> Bool
isSingleton node = Map.size (children' node) == 0

children :: (Ord dp) => CoverTree dp -> Map.Map dp (CoverTree dp)
children Leaf = error "children leaf"
children tree = children' tree `Map.union` Map.singleton (nodedp tree) (Node (nodedp tree) (level tree-1) mempty)

sepChildDist :: Fractional r => CoverTree dp -> r
sepChildDist node = 2^^(level node-1)

sepDist :: Fractional r => CoverTree dp -> r
sepDist node = 2^^(level node)

coverDist :: Fractional r => CoverTree dp -> r
coverDist node = 2^^(level node+1)

isCover :: (MetricSpace dp, Ord (Ring dp), Fractional (Ring dp)) => CoverTree dp -> dp -> Bool
isCover Leaf _ = False
isCover (Node p i set) q = distance p q < 2^^(i+1)

isCoverT :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    ) => CoverTree dp -> CoverTree dp -> Bool
isCoverT ct1 ct2 = distance (nodedp ct1) (nodedp ct2) + coverDist ct2 < coverDist ct1

---------------------------------------

insert :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Ord dp, Fractional (Ring dp)
    ) => CoverTree dp -> dp -> Maybe (CoverTree dp)
insert Leaf dp = Just (Node dp 10 mempty)
insert node dp = if distance dp (nodedp node) > sepDist node
    then Nothing
    else Just $ node
        { children' = if hasInsert
            then Map.insert key val $ children node
            else Map.insert dp (Node dp (level node-1) mempty) $ children node
        }
    where 
        viableChildren = Map.filter (\subtree -> isCover subtree dp) $ children node
        childrenInserts = Map.map (\tree -> insert tree dp) viableChildren

        insertables = Map.filterWithKey filtergo childrenInserts
        (key,Just val):xs = Map.assocs insertables

        hasInsert = Map.size insertables > 0

        filtergo _ Nothing   = False
        filtergo _ (Just _)  = True

insertBatch :: (MetricSpace dp, Ord (Ring dp), Ord dp, Fractional (Ring dp)) => [dp] -> CoverTree dp
insertBatch xs = go xs Leaf
    where
        go [] tree = tree
        go (x:xs) tree = go xs result
            where
                Just result = insert tree x

-------------------------------------------------------------------------------
-- algebra

instance
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    ) => Monoid (CoverTree dp) 
        where
    mempty = Leaf
    mappend Leaf Leaf = Leaf
    mappend Leaf node = node
    mappend node Leaf = node
    mappend ct1 ct2 = fromJust $ merge ct1 ct2
--     mappend ct1 ct2
--         | isCover ct1 ct2 = 

merge ct1 ct2 = merge' (growct ct1 maxlevel) (growct ct2 maxlevel)
    where
        maxlevel = max (level ct1) (level ct2)

merge' ct1 ct2 = assert "merge'" (level ct1==level ct2) $ if distance (nodedp ct1) (nodedp ct2) > sepDist ct1
    then Nothing
    else Just $ ct1
        { children' = go (children ct1) (Map.elems $ children ct2)
        }
    where
        go childmap [] = childmap
        go childmap (x:xs) = case catMaybesSnd $ map (\(k,v)->(k,merge' v x)) $ Map.assocs childmap of
            []           -> go (Map.insert (nodedp x) x childmap) xs
            (old,new):ys -> go (Map.insert (nodedp new) new $ Map.delete old childmap) xs

catMaybesSnd :: [(a,Maybe b)] -> [(a,b)]
catMaybesSnd [] = []
catMaybesSnd ((a,Nothing):xs) = catMaybesSnd xs
catMaybesSnd ((a,Just b):xs) = (a,b):catMaybesSnd xs

assert :: String -> Bool -> x -> x
assert str test x = if test
    then x
    else error $ "assert:"++str

map2 :: (a -> b -> c) -> [a] -> [b] -> [(b,[c])]
map2 f _ [] = []
map2 f xs (y:ys) = (y,map (\x -> f x y) xs):(map2 f xs ys)

calcOverlapDisjoint :: 
    ( MetricSpace dp
    , Ord dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Show dp
    ) => CoverTree dp -> CoverTree dp -> (Maybe (CoverTree dp), Maybe (CoverTree dp))
calcOverlapDisjoint ct1 ct2 = if ctMinDistance ct1 ct2 > 0
    then (Nothing, Just ct2)
    else if isCoverT ct1 ct2
        then (Just ct2, Nothing)
        else (list2ct $ catMaybes overlap, list2ct $ catMaybes disjoint)
    where
        (overlap,disjoint) = unzip $ map (calcOverlapDisjoint ct1) $ Map.elems $ childrenL

        childrenL = if Map.size (children' ct2) == 0
            then mempty
            else children ct2

-- | assumes that the input list is of disjoint CoverTrees all on the same level
-- list2ct :: (Eq dp) => [CoverTree dp] -> Maybe (CoverTree dp)
list2ct [] = Nothing
list2ct [x] = Just x
list2ct (x:xs) = trace ("mxlevel="++show maxlevel++", nodedpL="++show (map nodedp (x:xs))) $ assert $ Just $ Node
    { nodedp = nodedp x
    , level = level x+1
    , children' = Map.fromAscList $ map (\y -> (nodedp y, y {-growct y maxlevel-})) (x:xs)
    }
    where
        maxlevel = maximum $ map level (x:xs)


        levelcheck [] = True
        levelcheck [x] = True
        levelcheck (x:xs) = level x == level (head xs) && levelcheck xs

        assert :: a -> a
        assert a = if levelcheck $ Map.elems $ Map.fromAscList $ map (\y -> (nodedp y, growct y maxlevel)) (x:xs)
            then a
            else error "assert: list2ct"

growct :: CoverTree dp -> Int -> CoverTree dp
growct node i = if level node == i
    then node
    else growct node' i
    where 
        node'=Node
            { nodedp = nodedp node
            , level = level node+1
            , children' = Map.singleton (nodedp node) node
            }

prunect :: CoverTree dp -> CoverTree dp
prunect Leaf = Leaf
prunect node = if Map.size (children' node) == 1
    then prunect $ head $ Map.elems $ children' node
    else node

ctMinDistance :: (MetricSpace dp, Fractional (Ring dp)) => CoverTree dp -> CoverTree dp -> Ring dp
ctMinDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2) - ct1_adj - ct2_adj
    where
        ct1_adj = if isSingleton ct1
            then 0
            else coverDist ct1
        ct2_adj = if isSingleton ct2
            then 0
            else coverDist ct2

ctMaxDistance :: (MetricSpace dp, Fractional (Ring dp)) => CoverTree dp -> CoverTree dp -> Ring dp
ctMaxDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2)

-------------------------------------------------------------------------------
-- training

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    ) => HomTrainer (CoverTree dp) 
        where
    type Datapoint (CoverTree dp) = dp
    train1dp dp = Node dp 10 mempty

-------------------------------------------------------------------------------
-- tests

instance Arbitrary (CoverTree (Double,Double)) where
    arbitrary = do
        num :: Int <- choose (1,100)
        xs <- replicateM num $ do
            x <- choose (-2^^5,2^^5)
            y <- choose (-2^^5,2^^5)
            return (x,y)
        return $ insertBatch xs

property_covering :: 
    ( MetricSpace dp
    , Fractional (Ring dp)
    , Ord (Ring dp)
    , Ord dp
    ) => CoverTree dp -> Bool
property_covering Leaf = True
property_covering node = if Map.size (children' node) > 1 
    then maximum (map (distance (nodedp node) . nodedp) $ Map.elems $ children' node) < coverDist node 
      && and (map property_covering $ Map.elems $ children' node)
    else True

property_separation :: 
    ( MetricSpace dp
    , Fractional (Ring dp)
    , Ord (Ring dp)
    , Ord dp
    , Show (Ring dp)
    , Show dp
    ) => CoverTree dp -> Bool
property_separation Leaf = True
property_separation node = if Map.size (children' node) > 1
    then f $ minimum ((mapFactorial ctMaxDistance) $ Map.elems $ children node) > sepChildDist node
      && and (map property_separation $ Map.elems $ children' node)
    else True
    where
        f = trace ("mapFact="++show ((mapFactorial ctMaxDistance) $ Map.elems $ children node)++", sepDist="++show (sepDist node)++ "nodes="++show (Map.keys $ children node))

mapFactorial :: (a -> a -> b) -> [a] -> [b]
mapFactorial f [] = []
mapFactorial f (x:xs) = map (f x) xs ++ mapFactorial f xs

---------------------------------------

instance HasRing (Double,Double) where
    type Ring (Double,Double) = Double

instance MetricSpace (Double,Double) where
    distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^^2 + (y1-y2)^^2
         
randL :: Int -> IO [(Double,Double)]
randL n = replicateM n $ do
    x <- randomRIO (-100,100)
    y <- randomRIO (-100,100)
    return (fromIntegral (x :: Int), fromIntegral (y :: Int))

ys :: [(Double,Double)]
ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
zs = [(1,3),(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
mz = prunect $ insertBatch zs

-------------------------------------------------------------------------------
-- diagrams

draw Leaf = circle 1 # fc blue
draw tree = mkConnections $
                   (named (label++show depth) $ fontSize 0.5 $ text label <> circle 1 # fc red) 
               === (pad 1.05 $ centerName (label++show (depth-1)) $ 
                   Map.foldr (|||) mempty $ Map.map draw $ children' tree)
                
    where
        depth = level tree
        label = intShow $ nodedp tree

        mkConnections = connect (label++show depth) (label++show (depth-1) ) 
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth-1))) (Map.keys $ children tree))
            
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
