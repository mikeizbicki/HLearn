{-# LANGUAGE NoMonomorphismRestriction #-}

module HLearn.DataStructures.CoverTree
    where

import Control.Monad
import Control.Monad.Random
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Debug.Trace

import Diagrams.Prelude hiding (distance,trace)
import Diagrams.Backend.SVG.CmdLine

import HLearn.Algebra hiding ((#),(<>))

-------------------------------------------------------------------------------
-- data types

data CoverTree dp = Leaf | Node { nodedp :: dp, children' :: Map.Map dp (CoverTree dp) }
    deriving (Read,Show,Eq,Ord)

children :: (Ord dp) => CoverTree dp -> Map.Map dp (CoverTree dp)
children Leaf = error "children leaf"
children tree = children' tree `Map.union` Map.singleton (nodedp tree) (Node (nodedp tree) mempty)

isCover :: (MetricSpace dp, Ord (Ring dp), Fractional (Ring dp)) => CoverTree dp -> dp -> Int -> Bool
isCover Leaf _ _ = False
isCover (Node p set) q i = distance p q < 2^^i

-- insert :: (MetricSpace dp, Ord (Ring dp), Ord dp, Fractional (Ring dp)) => CoverTree dp -> dp -> Int -> Maybe (CoverTree dp)
insert Leaf dp _ = Just (Node dp mempty)
insert node dp i = if distance dp (nodedp node) > 2^^i 
    then Nothing
    else Just $ node
        { children' = if hasInsert
            then Map.insert key val $ children node
            else Map.insert dp (Node dp mempty) $ children node
        }
    where 
--         viableChildren = Map.filter (\subtree -> distance dp (nodedp subtree) < 2^i) $ children node
        viableChildren = Map.filter (\subtree -> isCover subtree dp i) $ children node
        childrenInserts = --trace ("\n\n\n\ni="++show i++", viableChildren="++show (Map.keys viableChildren)++", allchildren="++show (Map.keys $ children node)) $ 
            Map.map (\tree -> insert tree dp (i-1)) viableChildren

        insertables = Map.filterWithKey filtergo childrenInserts
        (key,Just val):xs = Map.assocs insertables

        hasInsert = Map.size insertables > 0

        filtergo _ Nothing   = False
        filtergo _ (Just _)  = True

-- insertBatch :: (MetricSpace dp, Ord (Ring dp), Ord dp, Fractional (Ring dp)) => [dp] -> CoverTree dp
insertBatch xs = go xs Leaf
    where
        go [] tree = tree
        go (x:xs) tree = go xs result
            where
                Just result = insert tree x 10


-------------------------------------------------------------------------------
-- tests

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
ys = [(1,1),(0,0),(1,-1),(0,1),(1,-1),(-1,-1),(1,1.5)]

zs :: [(Double,Double)]
zs = [(0,-1),(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]

-------------------------------------------------------------------------------
-- diagrams

draw depth Leaf = circle 1 # fc blue
draw depth tree = mkConnections $
                   (named (label++show depth) $ fontSize 0.5 $ text label <> circle 1 # fc red) 
               === (pad 1.05 $ centerName (label++show (depth+1)) $ Map.foldr (|||) mempty $ Map.map (draw (depth+1)) $ children' tree)
                
    where
        label = intShow $ nodedp tree

        mkConnections = connect (label++show depth) (label++show (depth+1) ) 
          . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (Map.keys $ children tree))
            
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
