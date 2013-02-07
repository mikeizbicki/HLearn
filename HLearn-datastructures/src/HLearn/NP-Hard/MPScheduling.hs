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