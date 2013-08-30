{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE BangPatterns, FlexibleContexts,FlexibleInstances,UndecidableInstances,TypeFamilies,ScopedTypeVariables #-}

module HLearn.DataStructures.CoverTreeNoopt
    where

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Hashable
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
-- import qualified Data.HashMap.Strict as Map
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
    { nodedp    :: !dp
    , sepdist   :: !(Ring dp)
    , children' :: !(Map.Map dp (CoverTree' dp)) 
    }

deriving instance (Show dp, Show (Ring dp)) => Show (CoverTree' dp)
deriving instance (Eq dp, Eq (Ring dp)) => Eq (CoverTree' dp)

instance NFData dp => NFData (CoverTree' dp) where
    rnf ct = deepseq (nodedp ct) $ rnf (children' ct)

instance NFData a => NFData (Option a) where
    rnf (Option Nothing) = ()
    rnf (Option (Just x)) = rnf x

isSingleton :: CoverTree' dp -> Bool
isSingleton node = Map.size (children' node) == 0

-- children :: (Ord dp,Hashable dp) => CoverTree' dp -> Map.Map dp (CoverTree' dp)
-- children tree = mapinsertSCCWith 
--     (\x y -> y) 
--     (nodedp tree) 
--     (Node (nodedp tree) (level tree-1) mempty)
--     (children' tree)
-- 
-- sepChildDist :: Fractional r => CoverTree' dp -> r
-- sepChildDist node = 2^^(level node-1)

-- sepDist :: Floating r => CoverTree' dp -> r
-- sepDist !node = 2**(fromIntegral $ level node)
-- sepDist !node = pow2 (level node)

pow2 :: Int -> Double
pow2 i = pow2vec VU.! (i-minindex) 

minindex= -100
pow2vec :: VU.Vector Double
pow2vec = VU.fromList $ map  (^^2) [minindex..100]

coverDist :: Fractional r => CoverTree' dp -> r
coverDist node = sepdist node *2 --2^^(level node+1)

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
-- insert node dp = if distance dp (nodedp node) > sepDist node
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
    , Hashable dp
    ) => Semigroup (CoverTree' dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = merge ct1 ct2

-- {-# INLINE merge #-}
merge ct1 ct2 = case merge' (growct ct1 maxlevel) (growct ct2 maxlevel) of
    Just x -> x
    Nothing -> merge (growct ct1 (maxlevel+1)) ct2
    where
        maxlevel = max (level ct1) (level ct2)

merge' !ct1 !ct2 = if distance (nodedp ct1) (nodedp ct2) > sepDist ct1
    then Nothing
    else Just $ ct1
        { children' = undefined -- go (children ct1) (mapelemsSCC $ children ct2)
        }
    where
        go !childmap ![] = childmap
        go !childmap !(x:xs) = case catMaybesSnd $ map (\(k,v)->(k,merge' v x)) $ mapassocsSCC childmap of
            []           -> go (mapinsertSCC (nodedp x) x childmap) xs
            (old,new):ys -> go (mapinsertSCC (nodedp new) new $ mapdeleteSCC old childmap) xs

mapinsertSCCWith = {-# SCC "Map.insertWith" #-} Map.insertWith
mapinsertSCC = {-# SCC "Map.insert" #-} Map.insert 
mapdeleteSCC = {-# SCC "Map.delete" #-} Map.delete
mapelemsSCC = {-# SCC "Map.elems" #-} Map.elems
-- mapassocsSCC = {-# SCC "Map.assocs" #-} Map.assocs
mapassocsSCC = {-# SCC "Map.assocs" #-} Map.toList

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
    , Hashable dp
    ) => HomTrainer (CoverTree dp) 
        where
    type Datapoint (CoverTree dp) = dp
    {-# INLINE train1dp #-}
    train1dp dp = Option $ Just $ Node dp 0 mempty

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

property_covering :: 
    ( MetricSpace dp
    , Fractional (Ring dp)
    , Ord (Ring dp)
    , Ord dp
    ) => CoverTree dp -> Bool
property_covering (Option Nothing) = True
property_covering (Option (Just node)) = if Map.size (children' node) > 1 
    then maximum (map (distance (nodedp node) . nodedp) $ mapelemsSCC $ children' node) < coverDist node 
      && and (map (property_covering . Option . Just) $ mapelemsSCC $ children' node)
    else True

property_separation :: 
    ( MetricSpace dp
    , Fractional (Ring dp)
    , Ord (Ring dp)
    , Ord dp
    , Show (Ring dp)
    , Show dp
    , Hashable dp
    ) => CoverTree dp -> Bool
property_separation (Option Nothing) = True
property_separation (Option (Just node)) = if Map.size (children' node) > 1
    then minimum ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children' node) > sepChildDist node
      && and (map (property_separation . Option . Just) $ mapelemsSCC $ children' node)
    else True
    where
        f = trace ("mapFact="++show ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children' node)++", sepDist="++show (sepDist node)++ "nodes="++show (Map.keys $ children' node))

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
-- my = prunect $ insertBatch ys
my = train ys :: CoverTree (Double,Double)

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
-- my' = prunect $ insertBatch ys'
my' = train ys' :: CoverTree (Double,Double)

zs :: [(Double,Double)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
-- mz = prunect $ insertBatch zs
mz = train zs :: CoverTree (Double,Double)

-------------------------------------------------------------------------------
-- diagrams

draw (Option (Just node)) = draw' 0 node
draw' depth tree = mkConnections $ 
                   (named (label++show depth) $ fontSize 0.5 $ 
                        (
                             (text label <> strutY 1) 
--                          === (text (justdouble (sepdist tree)) <> strutY 0.5)) 
                         )
                          <> circle 1 # fc red) 
               === (pad 1.05 $ centerName (label++show (depth+1)) $ 
                   Map.foldr (|||) mempty $ Map.map (draw' (depth+1)) $ children' tree)
                
    where
        label = intShow $ nodedp tree

        mkConnections = connect (label++show depth) (label++show (depth+1))
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (Map.keys $ children' tree))
-- draw tree = mkConnections $
--                    (named (label++show depth) $ fontSize 0.5 $ text label <> circle 1 # fc red) 
--                === (pad 1.05 $ centerName (label++show (depth-1)) $ 
--                    Map.foldr (|||) mempty $ Map.map draw $ children' tree)
--                 
--     where
--         depth = level tree
--         label = intShow $ nodedp tree
-- 
--         mkConnections = connect (label++show depth) (label++show (depth-1) ) 
--           . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth-1))) (Map.keys $ children tree))
            
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
