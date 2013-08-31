{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE BangPatterns, FlexibleContexts,FlexibleInstances,UndecidableInstances,TypeFamilies,ScopedTypeVariables #-}

module HLearn.DataStructures.CoverTree
    where

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU

import Test.QuickCheck
import Debug.Trace

import Diagrams.Prelude hiding (distance,trace)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>))

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = Option (CoverTree' dp)

data CoverTree' dp = Node 
    { nodedp     :: !dp
    , sepdist    :: !(Maybe (Ring dp))
--     , sepdistMin :: !(Ring dp)
--     , sepdistMax :: !(Ring dp)
    , children'  :: !(Map.Map dp (CoverTree' dp)) 
    }

deriving instance (Read (Ring dp), Read dp, Ord dp) => Read (CoverTree' dp)
deriving instance (Show (Ring dp), Show dp, Ord dp) => Show (CoverTree' dp)
deriving instance (Eq (Ring dp), Eq dp) => Eq (CoverTree' dp)
deriving instance (Ord (Ring dp), Ord dp) => Ord (CoverTree' dp)

instance NFData dp => NFData (CoverTree' dp) where
    rnf ct = deepseq (nodedp ct) $ rnf (children' ct)

instance NFData a => NFData (Option a) where
    rnf (Option Nothing) = ()
    rnf (Option (Just x)) = rnf x

isSingleton :: CoverTree' dp -> Bool
isSingleton node = Map.size (children' node) == 0

-- children = children'
children :: (Ord dp, Fractional (Ring dp)) => CoverTree' dp -> Map.Map dp (CoverTree' dp)
children tree = mapinsertSCCWith 
    (\x y -> y) 
    (nodedp tree) 
    (Node (nodedp tree) Nothing mempty)
    (children' tree)

-- sepChildDist :: (Fractional (Ring dp)) => CoverTree' dp -> Ring dp
-- sepChildDist node = sepdist node/2 

sepdistNum node = case sepdist node of
    Nothing -> 0.0000000000000000001
    Just x -> x

coverDist :: (Fractional (Ring dp)) => CoverTree' dp -> Ring dp
coverDist node = sepdistNum node*2 

ctMinDistance :: (MetricSpace dp, Fractional (Ring dp)) => CoverTree' dp -> CoverTree' dp -> Ring dp
ctMinDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2) - ct1_adj - ct2_adj
    where
        ct1_adj = if isSingleton ct1
            then 0
            else coverDist ct1
        ct2_adj = if isSingleton ct2
            then 0
            else coverDist ct2

ctMaxDistance :: (MetricSpace dp, Fractional (Ring dp)) => CoverTree' dp -> CoverTree' dp -> Ring dp
ctMaxDistance ct1 ct2 = distance (nodedp ct1) (nodedp ct2)

---------------------------------------

-- insert :: 
--     ( MetricSpace dp
--     , Ord (Ring dp)
--     , Ord dp, Fractional (Ring dp)
--     , Ring dp ~ Double
--     ) => CoverTree' dp -> dp -> Maybe (CoverTree' dp)
-- insert node dp = if distance dp (nodedp node) > sepdist node
--     then Nothing
--     else Just $ node
--         { children' = if hasInsert
--             then mapinsertSCC key val $ children node
--             else mapinsertSCC dp (Node dp (level node-1) mempty) $ children node
--         }
--     where 
--         viableChildren = Map.filter (\subtree -> isCover subtree dp) $ children node
--         childrenInserts = Map.map (\tree -> insert tree dp) viableChildren
-- 
--         insertables = Map.filterWithKey filtergo childrenInserts
--         (key,Just val):xs = mapassocsSCC insertables
-- 
--         hasInsert = Map.size insertables > 0
-- 
--         filtergo _ Nothing   = False
--         filtergo _ (Just _)  = True
-- 
-- insertBatch :: (MetricSpace dp, Ord (Ring dp), Ord dp, Fractional (Ring dp), Ring dp ~ Double) => [dp] -> CoverTree' dp
-- insertBatch (x:xs) = go xs $ Node x 10 mempty
--     where
--         go [] tree = tree
--         go (x:xs) tree = go xs result
--             where
--                 Just result = insert tree x

-------------------------------------------------------------------------------
-- algebra

instance
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    , Ring dp ~ Double
    , Show (CoverTree dp)
    , Show (dp)
    ) => Semigroup (CoverTree' dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = merge ct1 ct2 

-- {-# INLINE merge #-}
merge ct1 ct2 = if sepdistNum ct2>sepdistNum ct1
    then merge ct2 ct1
    else case merge' ct1 ct2 of
        Just x -> x
        Nothing -> fromJust $ -- trace ("ct1="++show ct1++", ct2="++show ct2) $ 
             merge' (updatesep ct1 ct2) ct2

updatesep :: 
    ( MetricSpace dp
    , RealFrac (Ring dp)
    , Floating (Ring dp)
    ) => CoverTree' dp -> CoverTree' dp -> CoverTree' dp
updatesep ct1 ct2 = ct1 { sepdist = Just $ minsep ct1 ct2 }
    where
        minsep ct1 ct2 = 2^^((ceiling $ log (distance (nodedp ct1) (nodedp ct2)) / (log 2) :: Int) )

merge' !ct1 !ct2 = if distance (nodedp ct1) (nodedp ct2) > sepdistNum ct1
    then Nothing
    else Just $ ct1
        { children' = go (children ct1) (mapelemsSCC $ children ct2)
        }
    where
        go !childmap ![] = childmap
        go !childmap !(x:xs) = case catMaybesSnd $ map (\(k,v)->(k,merge' v x)) $ mapassocsSCC childmap of
            []           -> trace "[]"$ go (mapinsertSCC (nodedp x) (updatesep x child) childmap) xs
            (old,new):ys -> trace ":" $ go (mapinsertSCC (nodedp new) (updatesep new child) $ mapdeleteSCC old childmap) xs
            where
                child = head $ Map.elems childmap

mapinsertSCCWith = {-# SCC "Map.insertWith" #-} Map.insertWith
mapinsertSCC = {-# SCC "Map.insert" #-} Map.insert 
mapdeleteSCC = {-# SCC "Map.delete" #-} Map.delete
mapelemsSCC = {-# SCC "Map.elems" #-} Map.elems
mapassocsSCC = {-# SCC "Map.assocs" #-} Map.assocs

catMaybesSnd :: [(a,Maybe b)] -> [(a,b)]
catMaybesSnd [] = []
catMaybesSnd ((a,Nothing):xs) = catMaybesSnd xs
catMaybesSnd ((a,Just b):xs) = (a,b):catMaybesSnd xs

-------------------------------------------------------------------------------
-- training

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
    , Ring dp ~ Double
    , Show dp
    ) => HomTrainer (CoverTree dp) 
        where
    type Datapoint (CoverTree dp) = dp
    {-# INLINE train1dp #-}
    train1dp dp = Option $ Just $ Node dp Nothing mempty

-------------------------------------------------------------------------------
-- tests

instance Arbitrary (CoverTree (Double,Double)) where
    arbitrary = do
        num :: Int <- choose (1,100)
        xs <- replicateM num $ do
            x <- choose (-2^^5,2^^5)
            y <- choose (-2^^5,2^^5)
            return (x,y)
        return $ train xs 

property_covering :: CoverTree (Double,Double) -> Bool
property_covering (Option Nothing) = True
property_covering (Option (Just node)) = if Map.size (children' node) > 1 
    then maximum (map (distance (nodedp node) . nodedp) $ mapelemsSCC $ children' node) < coverDist node 
      && and (map (property_covering . Option . Just) $ mapelemsSCC $ children' node)
    else True

property_leveled :: CoverTree (Double,Double) -> Bool
property_leveled (Option Nothing) = True
property_leveled (Option (Just node)) = case map sepdist (Map.elems $ children' node) of
    [] -> True
    xs -> all (== head xs) xs
       && and (map (property_leveled . Option . Just) $ mapelemsSCC $ children' node)

property_separating :: CoverTree (Double,Double) -> Bool 
property_separating (Option Nothing) = True
property_separating (Option (Just node)) = if Map.size (children' node) > 1
    then minimum ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children node) > (sepdistNum $ head $ Map.elems $ children' node)
      && and (map (property_separating . Option . Just) $ mapelemsSCC $ children' node)
    else True
    where
        f = trace ("mapFact="++show ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children node)++", sepdist="++show (sepdist node)++ "nodes="++show (Map.keys $ children node))

mapFactorial :: (a -> a -> b) -> [a] -> [b]
mapFactorial f [] = []
mapFactorial f (x:xs) = map (f x) xs ++ mapFactorial f xs

property_lossless :: [(Double,Double)] ->  Bool
property_lossless [] = True
property_lossless xs = Set.fromList xs == dpSet ct
    where
        Option (Just ct) = train xs :: CoverTree (Double,Double)

dpSet :: (Ord dp) => CoverTree' dp -> Set.Set dp
dpSet = Set.fromList . dpList
    where
        dpList :: CoverTree' dp -> [dp]
        dpList node = nodedp node:(concat . map dpList . mapelemsSCC $ children' node)

---------------------------------------

instance HasRing (Double,Double) where
    type Ring (Double,Double) = Double

instance MetricSpace (Double,Double) where
    {-# INLINE distance #-}
    distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
--     distance (x1,y1) (x2,y2) = (abs $ x1-x2) + (abs $ y1-y2)
         
randL :: Int -> IO [(Double,Double)]
randL n = replicateM n $ do
    x <- randomRIO (-100,100)
    y <- randomRIO (-100,100)
    return (fromIntegral (x :: Int), fromIntegral (y :: Int))

ys :: [(Double,Double)]
ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
my = train ys :: CoverTree (Double,Double)
-- my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
my' = train ys' :: CoverTree (Double,Double)
-- my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
mz = train zs :: CoverTree (Double,Double)
-- mz = prunect $ insertBatch zs

-------------------------------------------------------------------------------
-- diagrams

drawez (Option (Just node)) = draw node
draw tree = mkConnections $
                   (named (label++show depth) $ fontSize 0.5 $ text label <> circle 1 # fc red) 
               === (pad 1.05 $ centerName (label++show (depth/2)) $ 
                   Map.foldr (|||) mempty $ Map.map draw $ children' tree)
                
    where
        depth = sepdistNum tree
        label = intShow $ nodedp tree

        mkConnections = connect (label++show depth) (label++show (depth/2) ) 
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth/2))) (Map.keys $ children tree))
            
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
