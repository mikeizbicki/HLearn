module HLearn.DataStructures.KDTree
    where
    
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector as V
import Test.QuickCheck
import Debug.Trace

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

-- type DP = V.Vector Double
-- type family (Dimension dp)
-- 
-- (!) :: DP -> Dimension -> Double
-- (!) = (V.!)

class (Num (DimensionIndex x), Bounded (DimensionIndex x), Ord (DimensionIndex x), Ord (DimensionBase x)) => HasDimensions x where
    type DimensionIndex x 
    type DimensionBase x
    (!) :: x -> DimensionIndex x -> DimensionBase x

data KDTree dp
    = Leaf 
    | Node { val :: dp, splitdim :: DimensionIndex dp, left :: KDTree dp, right :: KDTree dp}
--     deriving (Read,Show,Eq,Ord)

deriving instance (Show dp, Show (DimensionIndex dp)) => Show (KDTree dp)

---------------------------------------

ppshow :: (Show dp) => KDTree dp -> String
ppshow = go [] 
    where
        go i Leaf = "" -- i++"*\n"
        go i t = i ++ " " ++ (show $ val t) ++ "\n" ++ go (i++"l") (left t) ++ go (i++"r") (right t)

basicshow :: (Show dp, Show (DimensionIndex dp)) => KDTree dp -> String
basicshow Leaf = "Leaf"
basicshow t = "Node { val="++show (val t)++", splitdim="++show (splitdim t)++", left="++go (left t)++", right="++go (right t)++" }"
    where
        go Leaf = "Leaf"
        go _ = "Node"


nextSplitDim :: (HasDimensions dp) => KDTree dp -> DimensionIndex dp
nextSplitDim t 
    | splitdim t == maxBound = minBound
    | otherwise = splitdim t+1

insert :: (HasDimensions dp) => KDTree dp -> dp -> KDTree dp
insert Leaf dp = Node dp 0 Leaf Leaf
insert t dp
    | dp ! splitdim t > val t ! splitdim t = t { right = insert (right t) dp }
    | dp ! splitdim t < val t ! splitdim t = t { left = insert (left t) dp }

isValid :: (HasDimensions dp) => KDTree dp -> Bool
isValid Leaf = True
isValid t    = thisvalid && isValid (left t) && isValid (right t)
    where
        thisvalid = go (left t) (\dp -> dp ! splitdim t <= val t ! splitdim t)
                 && go (right t) (\dp -> dp ! splitdim t > val t ! splitdim t)
         
        go Leaf f = True
        go t f = f (val t) && go (left t) f && go (right t) f

size :: KDTree dp -> Int
size Leaf = 0
size t = 1 + (size $ left t) + (size $ right t)

depth :: KDTree dp -> Int
depth Leaf = 0
depth t = 1+max (depth $ left t) (depth $ right t)

nodesAtDepth :: Int -> KDTree dp -> Int
nodesAtDepth _ Leaf = 0
nodesAtDepth 0 _ = 1
nodesAtDepth i t = nodesAtDepth (i-1) (left t) + nodesAtDepth (i-1) (right t)

balanceScore :: KDTree dp -> Double
balanceScore t = (fromIntegral $ depthSum t) / (fromIntegral $ balanceDepthSum $ size t)

depthSum :: KDTree dp -> Int
depthSum t = go 0 t 
    where
        go i Leaf = 0
        go i t = i+(go (i+1) (left t)) + (go (i+1) (right t))

balanceDepthSum :: Int -> Int 
balanceDepthSum i = fromIntegral $ go 0 i
    where
        go exp i
            | 2^exp < i = (exp+1)*2^exp + go (exp+1) (i-2^exp)
            | otherwise = (exp+1)*i

-------------------------------------------------------------------------------
-- algebra

debug _ _ _ action = action
-- debug pos a b action = trace (pos
--     ++"\n  ra="++(show $ right a)
--     ++"\n  rb="++(show $ right b)
--     ++"\n  la="++(show $ left a)
--     ++"\n  lb="++(show $ left b)
--     ++"\n  va="++(show $ val a)
--     ++"\n  vb="++(show $ val b)
--     ++"\n  sa="++(show $ splitdim a)
--     ++"\n  sb="++(show $ splitdim b)
--     )
--     action

instance (HasDimensions dp) => Monoid (KDTree dp) where
    mempty = Leaf
    mappend Leaf Leaf = Leaf
    mappend Leaf a = a
    mappend a Leaf = a
    mappend a@(Node _ _ Leaf Leaf) b@(Node _ _ Leaf Leaf)
        | val a ! splitdim a >  val b ! splitdim a = debug "a1" a b $ 
            Node (val a) (splitdim a) (Node (val b) (nextSplitDim a) Leaf Leaf) Leaf
        | val a ! splitdim a <= val b ! splitdim a = debug "a2" a b $ 
            Node (val a) (splitdim a) Leaf (Node (val b) (nextSplitDim a) Leaf Leaf)
    mappend a b@(Node _ _ Leaf Leaf) = insert a $ val b
    mappend a@(Node _ _ Leaf Leaf) b = insert b $ val a
    mappend a b = if  splitdim a == splitdim b 
        then if
            | val a ! splitdim a <  val b ! splitdim a -> debug "b1" a b $ right a <> b 
                { left = left a <> left b <> train1dp (val a) 
                } 
            | val a ! splitdim a >  val b ! splitdim a -> debug "b2" a b $ left a <> b 
                { right = right a <> right b <> train1dp (val a)
                } 
            | val a ! splitdim a == val b ! splitdim a -> debug "b3" a b $ b 
                { left = left a <> left b <> train1dp (val a)
                , right = right a <> right b 
                }
        else debug "c1" a b $ ((train1dp $ val a) {splitdim = splitdim a} ) <> (right a <> (left a <> b))
       
       
        
---------------------------------------

-- instance Functor KDTree where
--     fmap f Leaf = Leaf
--     fmap f t = Node 
--         { val = f $ val t
--         , splitdim = fromInteger $ toInteger $ splitdim t
--         , left = fmap f $ left t
--         , right = fmap f $ right t
--         }

instance F.Foldable KDTree where
    foldr f i Leaf = i
    foldr f i t = F.foldr f (F.foldr f (f (val t) i) (right t)) (left t)

class Prunable t where
    pfoldr :: (t a -> Bool) -> (a -> b -> b) -> b -> t a -> b

instance Prunable KDTree where
    pfoldr p f i t = if p t 
        then i
        else pfoldr p f (pfoldr p f (f (val t) i) (right t)) (left t) 

class (F.Foldable t) => DualFoldable t where
    dfoldr :: ((a,a) -> b -> b) -> b -> t a -> t a -> b
    dfoldr f i t1 t2 = foldr f i [(x,y) | x <- (F.toList t1), y <- (F.toList t2)]

-- instance T.Traversable KDTree where
    

-------------------------------------------------------------------------------
-- model

instance (HasDimensions dp) => HomTrainer (KDTree dp) where
    type Datapoint (KDTree dp) = dp
    train1dp dp = Node dp 0 Leaf Leaf

-------------------------------------------------------------------------------
-- testing

instance Arbitrary (V.Vector Double) where
    arbitrary = do
        a <- choose (-100,100)
        b <- choose (-100,100)
        return $ V.fromList $ [a,b]

instance (HasDimensions dp, Arbitrary dp) => Arbitrary (KDTree dp) where
    arbitrary = do
        xs <- arbitrary
        return $ train (xs :: [Datapoint (KDTree dp)])

instance HasDimensions (V.Vector Double) where
    type DimensionIndex (V.Vector Double) = Int
    type DimensionBase (V.Vector Double) = Double
    (!) = (V.!)

ds1 = map V.fromList [[1,5],[-1,4],[0,2],[2,-1],[-2,3],[-3,1]] :: [V.Vector Double]
ds2 = map V.fromList [[-1,5],[1,4],[1,2],[-2,-1],[-3,3],[-3,1]] :: [V.Vector Double]
m1 = train ds1 :: KDTree (V.Vector Double) 
m2 = train ds2 :: KDTree (V.Vector Double)
m' = foldl insert Leaf ds1

xs 1 = map V.fromList [[93,79],[15,35],[55,6]]
xs 2 = map V.fromList [[93,79],[55,6],[15,35]]
xs 3 = map V.fromList [[15,35],[93,79],[55,6]]
xs 4 = map V.fromList [[15,35],[55,6],[93,79]]
xs 5 = map V.fromList [[55,6],[93,79],[15,35]]
xs 6 = map V.fromList [[55,6],[15,35],[93,79]]
m i = train (xs i) :: KDTree (V.Vector Double)

[q1,q2,q3]=map train1dp $ xs 6 :: [KDTree (V.Vector Double)]


randmodel = fmap (train) $ replicateM 100000 (do
    x <- randomIO
    y <- randomIO
    return $ V.fromList [x::Double,y::Double]
    )
    :: IO (KDTree (V.Vector Double))
