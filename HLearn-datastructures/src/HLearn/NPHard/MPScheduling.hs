{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HLearn.NPHard.MPScheduling
    where

import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import qualified Data.Heap as Heap

import Debug.Trace
import GHC.TypeLits

import HLearn.Algebra

-------------------------------------------------------------------------------
-- helper types

data Task = Task { tasklen :: Integer }
    deriving (Read,Show,Eq,Ord)

data Schedule = Schedule
    { size :: Integer
    , tasks :: Seq.Seq Task
    }
    deriving (Read,Show,Eq)
    
instance Ord Schedule where
    compare s1 s2 = compare (size s1) (size s2)

instance Semigroup Schedule where
    s1 <> s2 = Schedule
        { size = (size s1)+(size s2)
        , tasks = (tasks s2)<>(tasks s2)
        }

instance Monoid Schedule where
    mempty = Schedule 0 mempty
    mappend = (<>)
    
instance Triangle Schedule Task where
    task <| sched = Schedule
        { size = (size sched) + tasklen task
        , tasks = task <| (tasks sched)
        }

    sched |> task = task <| sched

-------------------------------------------------------------------------------
-- MPSchedule

data MPSchedule (n::Nat) = MPSchedule
    { schedules :: Heap.MinHeap Schedule
    }
    deriving (Read,Show,Eq,Ord)
    
numProcessors :: forall n. SingI n => MPSchedule n -> Integer
numProcessors _ = fromSing (sing :: Sing n)
    
data MPScheduleParams (n::Nat) = MPScheduleParams
    deriving (Read,Show,Eq,Ord)

instance Model (MPScheduleParams n) (MPSchedule n) where
    getparams _ = MPScheduleParams
    
instance DefaultModel (MPScheduleParams n) (MPSchedule n) where
    defparams = MPScheduleParams
    
-------------------------------------------------------------------------------
-- algebra

instance Semigroup (MPSchedule n) where
    mps1 <> mps2 = undefined 
        
instance (SingI n) => Monoid (MPSchedule (n::Nat)) where
    mempty = MPSchedule mempty
--     mempty = MPSchedule $ V.replicate (fromIntegral $ fromSing (sing :: Sing n)) mempty
    mappend = (<>)
    
-------------------------------------------------------------------------------
-- training

instance (SingI n) => HomTrainer (MPScheduleParams n) Task (MPSchedule n) where
    train1dp' _ dp = MPSchedule $ Heap.singleton $ mempty |> dp
    
-------------------------------------------------------------------------------
-- useless helpers
newtype Partition' a (n::Nat) = Partition' { sets' :: (Map.Map Int (CountingSeq a)) }
    deriving (Read,Show,Eq,Ord)
    
partition2bst :: (Ord a) => Partition' a n -> BST (Double,a)
partition2bst p = train . F.toList $ countingseq $ Map.foldr (<>) mempty (sets' p) 

bst2partition' :: (Ord a) => BST (Double,a) -> Partition' a n
bst2partition' = F.foldr go (Partition' mempty)
    where
        go (w,a) (Partition' sets) = Partition' $  Map.insertWith (<>) 0 (singletoncs (w,a)) sets

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
