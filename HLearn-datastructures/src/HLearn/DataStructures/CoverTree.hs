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

import HLearn.Algebra hiding ((#),(<>),(|>))

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = Option (CoverTree' dp)

data CoverTree' dp = Node 
    { nodedp     :: !dp
    , sepdist    :: !(Ring dp)
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

coverDist :: (Fractional (Ring dp)) => CoverTree' dp -> Ring dp
coverDist node = sepdist node*2 

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
--     , Ring dp ~ Double
    , Show (CoverTree dp)
    , Show (dp)
    ) => Semigroup (CoverTree' dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = merge ct1 ct2 

merge ct1 ct2 = case merge' (growct ct1 maxlevel) (growct ct2 maxlevel) of
    Just x -> x
    Nothing -> merge (growct ct1 (maxlevel*2)) ct2
    where
        maxlevel = maximum [(sepdist ct1), (sepdist ct2),1]

merge' !ct1 !ct2 = if {-# SCC "if" #-} ({-# SCC "distance" #-} distanceFastBound (nodedp ct1) (nodedp ct2) (sepdist ct1)) > ({-# SCC "sepdist" #-} sepdist ct1)
    then {-# SCC "then" #-} Nothing
    else {-# SCC "else" #-} Just $ ct1
        { children' = {-# SCC "children'" #-} go (children ct1) (mapelemsSCC $ children ct2)
        }
    where
        go !childmap ![] = childmap
        go !childmap !(x:xs) = case filter (isJust . snd) $ map (\(k,v)->(k,merge' v x)) $ mapassocsSCC childmap of
            []                -> go (mapinsertSCC (nodedp x)   (x   {sepdist=sepdist ct1/2}) childmap) xs
            (old,Just new):ys -> go (mapinsertSCC (nodedp new) (new {sepdist=sepdist ct1/2}) $ mapdeleteSCC old childmap) xs

children :: (Ord dp,Fractional (Ring dp)) => CoverTree' dp -> Map.Map dp (CoverTree' dp)
children tree = mapinsertSCCWith 
    (\x y -> y) 
    (nodedp tree) 
    (Node (nodedp tree) (sepdist tree/2) mempty)
    (children' tree)

prunect :: CoverTree' dp -> CoverTree' dp
prunect ct = if Map.size (children' ct) == 1 
    then head $ Map.elems $ children' ct
    else ct

growct :: (Ord (Ring dp)) => CoverTree' dp -> Ring dp -> CoverTree' dp
growct ct d = if d > sepdist ct
    then ct
        { sepdist=d
        , children' = Map.singleton (nodedp ct) ct 
        }
    else ct

dist2up :: (Floating d,RealFrac d) => d -> d
dist2up d = dist2down d * 2

dist2down :: (Floating d,RealFrac d) => d -> d
dist2down d = 2^^(floor $ log d / log 2 :: Int)

-- merge ct1 ct2 = if sepdist ct1 == 0 && sepdist ct2 == 0
--     then ct1
--         { sepdist = sepdist'*2
--         , children' = Map.fromList
--             [ (nodedp ct1,ct1 { sepdist=sepdist'})
--             , (nodedp ct2,ct2 { sepdist=sepdist'})
--             ]
--         }
--     else case setup_merge' ct1 ct2 of
--        Nothing -> undefined
--        Just x -> x 
--     where
--         sepdist' = dist2down $ distance (nodedp ct1) (nodedp ct2)
--         
-- setup_merge' ct1 ct2 =
--     let ct1' = prunect ct1
--         ct2' = prunect ct2
--         (higher',lower') = if sepdist ct1' > sepdist ct2'
--             then (ct1',ct2')
--             else (ct2',ct1')
--         higher = higher'
--             { sepdist = sepdist higher
--             , children' = Map.singleton (nodedp higher') higher'
--             }
--         lower = lower'
--             { sepdist = sepdist higher
--             , children' = Map.singleton (nodedp lower') lower'
-- --             , children' = if Map.size (children' lower') > 0
-- --                 then Map.singleton (nodedp lower') lower'
-- --                 else mempty
--             }
--     in merge' higher lower
-- 
-- merge' ct1 ct2 = assert (sepdist ct1==sepdist ct2) "sepdist" $
--                  assert (Map.size (children' ct1) > 0) ("Map.size\n ct1="++show ct1++"\n ct2="++show ct2) $ 
--                  if distance (nodedp ct1) (nodedp ct2) > sepdist ct1
--     then Nothing
--     else Just $ ct1
--         { children' = go (children' ct1) ct2list -- (mapelemsSCC $ children' ct2)
--         }
--     where
--         ct2list = if Map.size (children' ct2) > 1 
--             then Map.elems $ children' ct2
--             else [ ct2 { sepdist = sepdist ct1 /2 } ]
--              
-- 
--         go childmap [] = childmap
--         go childmap (x:xs) = case filter (isJust . snd) $ map (\(k,v)->(k,setup_merge' v x)) $ mapassocsSCC childmap of
--             []                -> trace ("[] "++show (nodedp x)) $ go (mapinsertSCC (nodedp x) x childmap) xs
--             (old,Just new):ys -> trace (" : "++show (nodedp new)) $ 
--                 go (mapinsertSCC (nodedp new) new $ mapdeleteSCC old childmap) xs
--             where
--                 child = head $ Map.elems childmap

mapinsertSCCWith = {-# SCC "Map.insertWith" #-} Map.insertWith
mapinsertSCC = {-# SCC "Map.insert" #-} Map.insert 
mapdeleteSCC = {-# SCC "Map.delete" #-} Map.delete
mapelemsSCC = {-# SCC "Map.elems" #-} Map.elems
mapassocsSCC = {-# SCC "Map.assocs" #-} Map.assocs

assert cond str x
    | cond      = x
    | otherwise = error str

-------------------------------------------------------------------------------
-- training

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Ord dp
--     , Ring dp ~ Double
    , Show dp
    , Show (Ring dp)
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
--         xs <- replicateM num arbitrary
        xs <- replicateM num $ do
            x <- arbitrary
            y <- arbitrary
--             x <- choose (-2^^500,2^^500)
--             y <- choose (-2^^500,2^^500)
            trace ("(x,y)="++show (x,y)) $ return (x,y)
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
    then minimum ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children' node) > (sepdist $ head $ Map.elems $ children' node)
      && and (map (property_separating . Option . Just) $ mapelemsSCC $ children' node)
    else True
    where
        f = trace ("mapFact="++show ((mapFactorial ctMaxDistance) $ mapelemsSCC $ children' node)++", sepdist="++show (sepdist node)++ "nodes="++show (Map.keys $ children' node))

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

instance Num a => HasRing (a,a) where
    type Ring (a,a) = a

instance (RealFrac a, Floating a) => MetricSpace (a,a) where
--     {-# INLINE distance #-}
    distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

    distanceFastBound (x1,y1) (x2,y2) b = if pt1 > threshold
        then b
        else sqrt $ pt1 + (y1-y2)*(y1-y2)
        where
            pt1 = (x1-x2)*(x1-x2)
            threshold = b*b

    distanceFastMono (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
         
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
-- zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(20,10),(22,21)]
mz = train zs :: CoverTree (Double,Double)
-- mz = prunect $ insertBatch zs

-------------------------------------------------------------------------------
-- diagrams

drawT ct1 ct2 = draw ct1 
            ||| (text "<>" <> strutX 1.5)
            ||| draw ct2 
            ||| (text "=" <> strutX 1) 
            ||| (draw $ ct1 <> ct2)

draw (Option (Just node)) = draw' 0 node
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
