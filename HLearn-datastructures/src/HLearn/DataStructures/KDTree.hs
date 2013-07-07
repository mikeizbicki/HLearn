module HLearn.DataStructures.KDTree
    where
    
import qualified Data.Vector as V

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

type DP = V.Vector Double
type Dimension = Int

data KDTree
    = Leaf 
    | Node { val :: DP, splitdim :: Dimension, left :: KDTree, right :: KDTree }
    deriving (Read,Show,Eq,Ord)

---------------------------------------

(!) :: DP -> Dimension -> Double
(!) = (V.!)

nextSplitDim :: KDTree -> Dimension
nextSplitDim t
    | splitdim t >= (V.length . val) t -1 = 0
    | otherwise = splitdim t + 1

insert :: KDTree -> DP -> KDTree
insert Leaf dp = Node dp 0 Leaf Leaf
insert t dp
    | dp ! splitdim t > val t ! splitdim t = t { right = insert (right t) dp }
    | dp ! splitdim t < val t ! splitdim t = t { left = insert (left t) dp }

validKDTree :: KDTree -> Bool
validKDTree Leaf = True
validKDTree t    = thisvalid && validKDTree (left t) && validKDTree (right t)
    where
        thisvalid = go (left t) (\dp -> dp ! splitdim t < val t ! splitdim t)
                 && go (right t) (\dp -> dp ! splitdim t > val t ! splitdim t)
         
        go Leaf f = True
        go t f = f (val t) && go (left t) f && go (right t) f

-------------------------------------------------------------------------------
-- algebra

instance Monoid KDTree where
    mempty = Leaf
    mappend Leaf Leaf = Leaf
    mappend Leaf a = a
    mappend a Leaf = a
    mappend a b
        | splitdim a == splitdim b && val a ! splitdim a < val b ! splitdim b = Node 
            { val = val b
            , splitdim = splitdim b
            , left = a <> left b
            , right = right b
            }
        | splitdim a == splitdim b && val a ! splitdim a > val b ! splitdim b = Node
            { val = val b
            , splitdim = splitdim b
            , left = left b
            , right = a <> right b
            }
        | otherwise = error $ "KDTree.mappend: a=" ++ show a ++ ", b="++show b

---------------------------------------

-- instance Foldable KDTree

-------------------------------------------------------------------------------
-- model

instance HomTrainer KDTree where
    type Datapoint KDTree = DP
    train1dp dp = Node dp 0 Leaf Leaf

-------------------------------------------------------------------------------
-- testing

xs = map V.fromList [[1,5],[-1,4],[0,2],[2,-1],[-2,3],[-3,1]]
m = train xs :: KDTree
m' = foldl insert Leaf xs
