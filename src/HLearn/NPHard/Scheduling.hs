{-# LANGUAGE DataKinds #-}

-- | See the wikipedia article for details about the Multiprocessor Scheduling problem <https://en.wikipedia.org/wiki/Multiprocessor_scheduling>

module HLearn.NPHard.Scheduling
    (
    
    Scheduling (..)

    -- * Operations
    , getSchedules
    , maxpartition
    , minpartition
    , spread
    ) where
          
import Control.DeepSeq
import qualified Control.ConstraintKinds as CK
import qualified Data.Foldable as F
import qualified Data.Heap as Heap
import Data.List
import Data.List.Extras
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import GHC.TypeLits
import HLearn.Algebra
import HLearn.DataStructures.SortedVector

-------------------------------------------------------------------------------
-- data types    

type Bin = Int

data Scheduling (n::Nat) a = Scheduling
    { vector   :: !(SortedVector a)
    , schedule :: Map.Map Bin [a]
    }
    deriving (Read,Show,Eq,Ord)

instance NFData a => NFData (Scheduling n a) where
    rnf s = deepseq (vector s)
          $ rnf (schedule s)

---------------------------------------

lptf :: forall a n. (Norm a, Ord (Ring a), SingI n) => SortedVector a -> Scheduling n a
lptf vector = Scheduling
    { vector = vector
    , schedule = vector2schedule (fromIntegral $ fromSing (sing :: Sing n)) vector
    }

vector2schedule :: (Norm a, Ord (Ring a)) => Bin -> SortedVector a -> Map.Map Bin [a]
vector2schedule p vector = snd $ CK.foldr cata (emptyheap p,Map.empty) vector
    where
        -- maintain the invariant that size of our heap is always p
        -- the processor with the smallest workload is at the top
        cata x (heap,map) = 
            let Just top = Heap.viewHead heap
                set = snd top
                prio = (fst top)+magnitude x
                heap' = Heap.insert (prio,set) (Heap.drop 1 heap)
                map' = Map.insertWith (++) set [x] map
            in (heap',map')

emptyheap :: (Num ring, Ord ring) => Bin -> Heap.MinPrioHeap ring Bin
emptyheap p = Heap.fromAscList [(0,i) | i<-[1..p]]

---------------------------------------

-- | Returns a list of all schedules.  The schedules are represented by a list of the elements within them.
getSchedules :: Scheduling n a -> [[a]]
getSchedules = Map.elems . schedule

-- | Returns the size of the largest bin
maxpartition :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
maxpartition p = maximum $ map (sum . map magnitude) $ Map.elems $ schedule p

-- | Returns the size of the smallest bin
minpartition :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
minpartition p = minimum $ map (sum . map magnitude) $ Map.elems $ schedule p

-- | A schedule's spread is a measure of it's \"goodness.\"  The smaller the spread, the better the schedule.  It is equal to `maxpartition` - `minpartition`
spread :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
spread p = (maxpartition p)-(minpartition p)

-------------------------------------------------------------------------------
-- Algebra

instance (Ord a, Ord (Ring a), Norm a, SingI n) => Abelian (Scheduling n a) 
instance (Ord a, Ord (Ring a), Norm a, SingI n) => Monoid (Scheduling n a) where
    mempty = lptf mempty
    p1 `mappend` p2 = lptf $ (vector p1) <> (vector p2)

instance (Ord a, Ord (Ring a), Norm a, SingI n, Group (SortedVector a)) => Group (Scheduling n a) where
    inverse p = Scheduling
        { vector = inverse $ vector p
        , schedule = error "Scheduling.inverse: schedule does not exist for inverses"
        }

instance (HasRing (SortedVector a)) => HasRing (Scheduling n a) where
    type Ring (Scheduling n a) = Ring (SortedVector a)

instance (Ord a, Ord (Ring a), Norm a, SingI n, Module (SortedVector a)) => Module (Scheduling n a) where
    r .* p = p { vector = r .* vector p }

---------------------------------------

instance CK.Functor (Scheduling n) where
    type FunctorConstraint (Scheduling n) x = (Ord x, Norm x, SingI n)
    fmap f sched = lptf $ CK.fmap f $ vector sched

-- instance CK.Monad (Scheduling n) where
--     return = train1dp
--     join (Scheduling v _) = lptf $ CK.join $ CK.fmap vector v

-------------------------------------------------------------------------------
-- Training

instance (Ord a, Ord (Ring a), Norm a, SingI n) => HomTrainer (Scheduling n a) where
    type Datapoint (Scheduling n a) = a
    train1dp dp = lptf $ train1dp dp
    train dp = lptf $ train dp
    
-------------------------------------------------------------------------------
-- Visualization

class Labeled a where
    label :: a -> String

rmblankline :: String -> String
rmblankline [] = []
rmblankline ('\n':'\n':xs) = rmblankline ('\n':xs)
rmblankline (x:xs) = x:(rmblankline xs)

visualize :: (Norm a, Labeled a, Show (Ring a), Fractional (Ring a)) => Ring a -> Map.Map Bin [a] -> String
visualize height m = rmblankline $ unlines
    [ "\\begin{tikzpicture}"
    , "\\definecolor{hlearn_bgbox}{RGB}{127,255,127}"
    , unlines $ map mknodes $ Map.assocs m
    , unlines $ map (mkproc height) $ Map.assocs m
    , "\\end{tikzpicture}"
    ]

-- mkproc :: (k,[a]) -> String
mkproc height (k,xs) = 
    "\\draw[line width=0.1cm] ("++show x++","++show height++") to ("++show x++",0) to node[below] {$s_{"++show k++"}$} ("++show x++"+2,0) to ("++show x++"+2,"++show height++");"
    where
        x = k*2

-- mknodes :: (k,[a]) -> String
mknodes (k,xs) = unlines $ go 0 (reverse xs)
    where
        go i [] = [""]
        go i (x:xs) = (node (k*2+1) (i+(magnitude x)/2) (magnitude x) (label x)):(go (i+magnitude x) xs)
            
    
-- node :: Double->Double->Double->String->String
node x y height name = 
    "\\node[shape=rectangle,draw,fill=hlearn_bgbox,minimum width=2cm,minimum height="++show height++"cm] at ("++show x++","++show y++") { "++name++" };"
