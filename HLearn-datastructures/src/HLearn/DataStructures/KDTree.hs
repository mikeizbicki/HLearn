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

class 
    ( Num (DimensionIndex x)
    , Bounded (DimensionIndex x)
    , Ord (DimensionIndex x)
    , Ord (DimensionBase x)
    ) => HasDimensions x 
        where
    type DimensionIndex x 
    type DimensionBase x
    (!) :: x -> DimensionIndex x -> DimensionBase x

data KDTree dp
    = Leaf 
    | Node { val :: dp, splitdim :: DimensionIndex dp, left :: KDTree dp, right :: KDTree dp}

deriving instance (Show dp, Show (DimensionIndex dp)) => Show (KDTree dp)
deriving instance (Read dp, Read (DimensionIndex dp)) => Read (KDTree dp)

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
    | dp ! splitdim t >  val t ! splitdim t = t { right = insert (right t) dp }
    | dp ! splitdim t <= val t ! splitdim t = t { left = insert (left t) dp }

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

instance F.Foldable KDTree where
    foldr f i Leaf = i
    foldr f i t = F.foldr f (F.foldr f (f (val t) i) (right t)) (left t)

class Prunable t selector | t -> selector where
    pfoldr :: (b -> t a ->             Bool) -> (a -> b -> b) -> b -> t a -> b
    qfoldr :: (b -> t a -> selector -> Bool) -> (a -> b -> b) -> b -> t a -> b

data Selector_KDTree = TH_Right | TH_Left

instance Prunable KDTree Selector_KDTree where
    pfoldr p f i Leaf = i
    pfoldr p f i t = if p i t 
        then pfoldr p f (pfoldr p f (f (val t) i) (right t)) (left t) 
        else i

    qfoldr p f i Leaf = i
    qfoldr p f i t = if p i t TH_Left
        then qfoldr p f dorightfold (left t)
        else dorightfold
        where
            dorightfold = if p i t TH_Right
                then qfoldr p f (f (val t) i) (right t)
                else i

class (F.Foldable t) => DualFoldable t where
    dfoldr :: ((a,a) -> b -> b) -> b -> t a -> t a -> b
    dfoldr f i t1 t2 = foldr f i [(x,y) | x <- (F.toList t1), y <- (F.toList t2)]

---------------------------------------
-- space algorithms

nn_basecase :: (Ord (Ring dp), MetricSpace dp) => dp -> dp -> Maybe dp -> Maybe dp 
nn_basecase query reference Nothing = Just reference
nn_basecase query reference (Just curbest) = Just $ if distance query curbest > distance query reference
    then reference
    else curbest 

nn_score :: Maybe dp -> KDTree dp -> Bool
nn_score Nothing t = True
nn_score (Just query) t = True

mindist :: 
    ( HasDimensions dp
    , DimensionBase dp ~ Ring dp
    , MetricSpace dp
    , Ord (Ring dp)
    , Show dp, Show (Ring dp), Show (DimensionIndex dp)
    ) => dp -> KDTree dp -> (Ring dp, dp)
mindist query Leaf = error "KDTree.mindist on Leaf"
mindist query t = qfoldr prune cata start t
    where
        prune (dist,dp) t TH_Left  = (query ! splitdim t) - (val t ! splitdim t) < dist 
        prune (dist,dp) t TH_Right = (val t ! splitdim t) - (query ! splitdim t) < dist

        cata a (dist,dp) = if dist > distance query a
            then (distance query a, a)
            else (dist,dp)

        start = (distance query (val t), val t) 

mindist_noprune :: 
    ( MetricSpace dp
    , Ord (Ring dp)
    ) => dp -> KDTree dp -> (Ring dp, dp)
mindist_noprune query t = F.foldr cata start t
    where
        cata a (dist,dp) = if dist > distance query a
            then (distance query a, a)
            else (dist,dp)

        start = (distance query (val t), val t) 

-- mindistworks dp = mindist dp m2 == mindist_noprune dp m2
mindistworks (dp,t :: KDTree (Double,Double)) = 
    depth t > 0 ==> mindist dp t == mindist_noprune dp t

-- mindist :: (MetricSpace dp, Ord (Ring dp)) => dp -> KDTree dp -> Ring dp
-- mindist query Leaf = error "KDTree.mindist on Leaf"
-- mindist query t = minimum $ (getdist $ left t) ++ (getdist $ right t) ++  [distance query (val t)] 
--     where
--         getdist Leaf = []
--         getdist t = [mindist query t]
--         leftdist = if left t == Leaf
--             then []
--             else [mindist query (left t)]
--         rightdist = if right t == Leaf
--             then []
--             else [mindist query (right t)]

nn :: (Ord (Ring dp), MetricSpace dp) => dp -> KDTree dp -> Maybe dp
nn query t = pfoldr nn_score (nn_basecase query) Nothing t

nn_noprune :: (Ord (Ring dp), MetricSpace dp) => dp -> KDTree dp -> Maybe dp
nn_noprune query t = F.foldr (nn_basecase query) Nothing t

-------------------------------------------------------------------------------
-- model

instance (HasDimensions dp) => HomTrainer (KDTree dp) where
    type Datapoint (KDTree dp) = dp
    train1dp dp = Node dp 0 Leaf Leaf

-------------------------------------------------------------------------------
-- testing

instance Arbitrary (Double,Double) where
    arbitrary = do
        a <- choose (-100,100)
        b <- choose (-100,100)
        return (a,b) 

instance (HasDimensions dp, Arbitrary dp) => Arbitrary (KDTree dp) where
    arbitrary = do
        xs <- arbitrary
        return $ train (xs :: [Datapoint (KDTree dp)])

class DependentIndex datatype index result | datatype index -> result where
    (#) :: datatype -> index -> result

data TI_1 = TI_1
data TI_2 = TI_2
data TI_3 = TI_3

class Index datatype index result | datatype -> index result where
    (##) :: datatype -> index -> result

-- instance Index (a,a) Index_Tuple2 a where
--     (##) (a0,a1) 0 = a0
--     (##) (a0,a1) 1 = a1

data Index_Tuple2 = Index_Tuple2_1 | Index_Tuple2_2

instance DependentIndex (a,b) TI_1 a where (a,b) # TI_1 = a
instance DependentIndex (a,b) TI_2 b where (a,b) # TI_2 = b

class CompareTypeLens base index where
    compareTL :: base -> base -> index -> Bool 

instance (Ord a, Ord b) => CompareTypeLens (a,b) Index_Tuple2 where
    compareTL b1 b2 Index_Tuple2_1 = b1 # TI_1 < b2 # TI_1
    compareTL b1 b2 Index_Tuple2_2 = b1 # TI_2 < b2 # TI_2

instance (Ord a) => HasDimensions (a,a) where
    type DimensionIndex (a,a) = Int
    type DimensionBase (a,a) = a

    (!) (a0,a1) 0 = a0
    (!) (a0,a1) 1 = a1

instance (Num a) => HasRing (a,a) where
    type Ring (a,a) = a

instance (Floating a) => MetricSpace (a,a) where
    distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

instance DependentIndex (a,b,c) TI_1 a where (a,b,c) # TI_1 = a
instance DependentIndex (a,b,c) TI_2 b where (a,b,c) # TI_2 = b
instance DependentIndex (a,b,c) TI_3 c where (a,b,c) # TI_3 = c

randmodel len = fmap (train) $ replicateM len $ do
    x <- randomIO
    y <- randomIO
    return (x,y) 
    :: IO (KDTree (Double,Double))
