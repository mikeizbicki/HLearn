{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import Data.Csv
import System.IO
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Map as Map
import qualified Data.Vector  as V

import HLearn.Algebra
import HLearn.NPHard.Scheduling
import HLearn.NPHard.BinPacking

data Task = Task
   { taskid :: String
   , cost   :: Double
   }
   deriving (Read,Show,Eq)

instance HasRing Task where
   type Ring (Task) = Double

instance Norm Task where
   magnitude = cost
   
instance Labeled Task where
    label = taskid

instance Ord Task where
    compare t1 t2 = compare (cost t1) (cost t2)

mkdataset l xs = [ Task ("$t_{"++l++show i++"}$") t | (i,t) <- zip [1..] xs]

fig1_tasks = [2,2.1,1.5,0.65,0.6,0.7,0.8,0.9,1.3,1.7]
fig1_model = train (mkdataset "" fig1_tasks) :: Scheduling 4 Task
fig1 = putStr $ visualize 4 $ schedule fig1_model
    

    
fig_binpack_tasks = Map.fromList 
    [(1,[Task {taskid = "$t_{6}$", cost = 0.7},Task {taskid = "$t_{2}$", cost = 2.1}])
    ,(2,[Task {taskid = "$t_{7}$", cost = 0.8},Task {taskid = "$t_{1}$", cost = 2.0}])
    ,(3,[Task {taskid = "$t_{8}$", cost = 0.9},Task {taskid = "$t_{10}$", cost = 1.7}])
    ,(4,[Task {taskid = "$t_{9}$", cost = 1.3},Task {taskid = "$t_{3}$", cost = 1.5}])
    ,(5,[Task {taskid = "$t_{5}$", cost = 0.6},Task {taskid = "$t_{4}$", cost = 0.65}])
    ]
fig_binpack = putStr $ visualize 3.6 fig_binpack_tasks
    
fig_mon_tasksA = [2.1,2.7,3.1,1.7,1.6,0.6]
fig_mon_modelA = train (mkdataset "a," fig_mon_tasksA) :: Scheduling 3 Task
fig_monA = putStr $ visualize 7 $ schedule fig_mon_modelA

fig_mon_tasksB = [0.6,2.5,1.0,0.7,0.65,0.7,0.9,0.85]
fig_mon_modelB = train (mkdataset "b," fig_mon_tasksB) :: Scheduling 3 Task
fig_monB = putStr $ visualize 7 $ schedule fig_mon_modelB

fig_mon_model = fig_mon_modelA<>fig_mon_modelB
fig_mon = putStr $ visualize 7 $ schedule fig_mon_model
