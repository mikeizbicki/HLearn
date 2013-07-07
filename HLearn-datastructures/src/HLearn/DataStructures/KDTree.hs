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

ppshow :: KDTree -> String
ppshow = go 0
    where
        go i Leaf = ""
        go i t = replicate i ' ' ++ (show $ val t) ++ "\n" ++ go (i+1) (left t) ++ go (i+1) (right t)

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

isValid :: KDTree -> Bool
isValid Leaf = True
isValid t    = thisvalid && isValid (left t) && isValid (right t)
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
    mappend a b = if
        | splitdim a == splitdim b -> if
            | val a ! splitdim a <  val b ! splitdim b -> b 
                { left = left a <> left b <> train1dp (val a) 
                } <> right a 
            | val a ! splitdim a >  val b ! splitdim b -> b 
                { right = right a <> right b <> train1dp (val a)
                } <> left a
            | val a ! splitdim a == val b ! splitdim b -> b 
                { left = left a <> left b <> train1dp (val a)
                , right = right a <> right b 
                } 
        | otherwise -> error $ "KDTree.mappend: a=" ++ show a ++ ", b="++show b

---------------------------------------

-- instance Foldable KDTree

-------------------------------------------------------------------------------
-- model

instance HomTrainer KDTree where
    type Datapoint KDTree = DP
    train1dp dp = Node dp 0 Leaf Leaf

-------------------------------------------------------------------------------
-- testing

ds1 = map V.fromList [[1,5],[-1,4],[0,2],[2,-1],[-2,3],[-3,1]]
ds2 = map V.fromList [[-1,5],[1,4],[1,2],[-2,-1],[-3,3],[-3,1]]
m1 = train ds1 :: KDTree
m2 = train ds2 :: KDTree
m' = foldl insert Leaf ds1
