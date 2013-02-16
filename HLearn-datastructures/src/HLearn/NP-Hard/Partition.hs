{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HLearn.NPHard.Partition
    where
          
import qualified Data.Foldable as F
import Data.List
import Data.List.Extras
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import GHC.TypeLits
import HLearn.Algebra
import HLearn.DataStructures.BinarySearchTree

-------------------------------------------------------------------------------
-- useless helpers

data CountingSeq a = CountingSeq 
    { total :: Double
    , countingseq :: Seq.Seq (Double,a)
    }
    deriving (Read,Show,Eq,Ord)
    
cs2list :: CountingSeq a -> [(Double,a)]
cs2list = F.toList . countingseq
    
singletoncs :: (Double,a) -> CountingSeq a
singletoncs (w,a) = CountingSeq w $ Seq.singleton (w,a)
    
instance F.Foldable CountingSeq where
    foldr f b (CountingSeq t s) = F.foldr (f . snd) b s
    
instance Triangle (CountingSeq a) (Double,a) where
    (CountingSeq t s) |> (d,a) = CountingSeq (t+d) (s|>(d,a))
    (d,a) <| (CountingSeq t s) = CountingSeq (t+d) ((d,a)<|s)
    
instance Semigroup (CountingSeq a) where
    (CountingSeq t1 s1)<>(CountingSeq t2 s2)=CountingSeq (t1+t2) (s1<>s2)
    
instance Monoid (CountingSeq a) where
    mempty = CountingSeq 0 mempty
    mappend = (<>)
    
newtype Partition' a (n::Nat) = Partition' { sets' :: (Map.Map Int (CountingSeq a)) }
    deriving (Read,Show,Eq,Ord)
    
partition2bst :: (Ord a) => Partition' a n -> BST (Double,a)
partition2bst p = train . F.toList $ countingseq $ Map.foldr (<>) mempty (sets' p) 

bst2partition' :: (Ord a) => BST (Double,a) -> Partition' a n
bst2partition' = F.foldr go (Partition' mempty)
    where
        go (w,a) (Partition' sets) = Partition' $  Map.insertWith (<>) 0 (singletoncs (w,a)) sets


-------------------------------------------------------------------------------
-- data types
    

data Partition a (n::Nat) = Partition
    { bst :: !(BST (Double,a))
    , sets :: Map.Map Int [(Double,a)]
    }
    deriving (Read,Show,Eq,Ord)
    
testp = train [(4,"poop"),(3,"the"),(1,"fart"),(1,"fart")] :: Partition String 30
    
-- bst2partition :: forall n a. (SingI n) => BST (Double,a) -> Partition a n
bst2partition :: forall n a. (SingI n) => BST (Double,a) -> Partition a n
bst2partition bst = Partition
    { bst = bst
--     , sets = undefined
    , sets = bst2sets (fromIntegral $ fromSing (sing :: Sing n)) bst
    }
    
bst2sets :: Int -> BST (Double,a) -> Map.Map Int [(Double,a)]
bst2sets n bst = Map.map cs2list $ F.foldr go base bst
    where
        base = Map.fromList [(i,mempty) | i<-[0..n-1]]
--         base = Map.fromList [(i,(0::Double,[]::[a])) | i<-[0..fromIntegral $ fromSing (sing :: Sing n)-1]]
        go (w,a) sets = Map.insertWith (<>) minindex (singletoncs (w,a)) sets
            where
                minindex = fst $ argmin (\(k,p) -> total p) $ Map.toList sets
--                 minindex = 0
    
data PartitionParams = PartitionParams
    deriving (Read,Show,Eq,Ord)

-- 
-- data PartitionedSet a (n::Nat) = PartitionedSet
--     { getsets :: Map.Map Int (Double,[a])
--     }
--     deriving (Read,Show,Eq,Ord)
--     
-- emptypartionedset :: forall n a. (SingI n) => PartitionedSet a (n::Nat)
-- emptypartionedset = PartitionedSet $ Map.fromList [(i,(0::Double,[]::[a])) | i<-[0..fromIntegral $ fromSing (sing :: Sing n)-1]]
--     
-- data PartitionedSetParams = PartitionedSetParams
--     deriving (Read,Show,Eq,Ord)
--     
-- instance Model PartitionedSetParams (PartitionedSet a n) where
--     getparams _ = PartitionedSetParams
--     
-- instance (Show a)=>LameTrainerOnline PartitionedSetParams (Double,a) (PartitionedSet a n) where
--     lame_add1dp model (size,dp) = model
--         { getsets = Map.insertWith (\(size1,x:[]) (size2,xs) -> (size1+size2,x:xs)) minindex (size,[dp]) (getsets model)
--         }
--         where
--             minindex = fst $ argmin (\(k,(w,dp)) -> w) $ Map.toList $ getsets model
-- 
-- testm = lame_addBatch (emptypartionedset :: PartitionedSet Int 2) 
--     [(2::Double,2::Int), (3,3), (4,4)]

-------------------------------------------------------------------------------
-- Algebra

instance (Ord a, SingI n) => Semigroup (Partition a n) where
    p1 <> p2 = bst2partition $ (bst p1) <> (bst p2)

instance (Ord a, SingI n) => Monoid (Partition a n) where
    mempty = bst2partition mempty
    mappend = (<>)

-------------------------------------------------------------------------------
-- Training

instance Model PartitionParams (Partition a n) where
    getparams _ = PartitionParams
    
instance (SingI n) => DefaultModel PartitionParams (Partition a n) where
    defparams = PartitionParams
    
instance (Ord a, SingI n) => HomTrainer PartitionParams (Double,a) (Partition a n) where
    train1dp' _ dp = bst2partition $ train1dp dp