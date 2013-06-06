module HLearn.NPHard.Scheduling
    where
          
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

data Scheduling (n::Nat) a = Scheduling
    { vector      :: !(SortedVector a)
    , schedule :: Map.Map Int [a]
    }
    deriving (Read,Show,Eq,Ord)

vector2scheduling :: forall a n. (Show a, Show (Ring a), Norm a, Ord (Ring a), SingI n) => SortedVector a -> Scheduling n a
vector2scheduling vector = Scheduling
    { vector = vector
    , schedule = lptf (fromIntegral $ fromSing (sing :: Sing n)) vector
    }

-- | the Least Processing Time First approximation; takes as input a presorted vector
lptf :: (Show a, Show (Ring a), Norm a, Ord (Ring a)) => Int -> SortedVector a -> Map.Map Int [a]
lptf p vector = snd $ F.foldr cata (emptyheap p,Map.empty) vector
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

emptyheap :: (Num ring, Ord ring) => Int -> Heap.MinPrioHeap ring Int
emptyheap p = Heap.fromAscList [(0,i) | i<-[1..p]]

---------------------------------------

getSchedules :: Scheduling n a -> [[a]]
getSchedules = Map.elems . schedule

maxpartition :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
maxpartition p = maximum $ map (sum . map magnitude) $ Map.elems $ schedule p

minpartition :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
minpartition p = minimum $ map (sum . map magnitude) $ Map.elems $ schedule p

spread :: (Ord (Ring a), Norm a) => Scheduling n a -> Ring a
spread p = (maxpartition p)-(minpartition p)

-------------------------------------------------------------------------------
-- Algebra

instance (Show a, Show (Ring a), Ord a, Ord (Ring a), Norm a, SingI n) => Monoid (Scheduling n a) where
    mempty = vector2scheduling mempty
    p1 `mappend` p2 = vector2scheduling $ (vector p1) <> (vector p2)

-------------------------------------------------------------------------------
-- Training

instance (Show a, Show (Ring a), Ord a, Ord (Ring a), Norm a, SingI n) => HomTrainer (Scheduling n a) where
    type Datapoint (Scheduling n a) = a
    train1dp dp = vector2scheduling $ train1dp dp
    
-------------------------------------------------------------------------------
-- Visualization

class Labeled a where
    label :: a -> String

rmblankline :: String -> String
rmblankline [] = []
rmblankline ('\n':'\n':xs) = rmblankline ('\n':xs)
rmblankline (x:xs) = x:(rmblankline xs)

visualize :: (Norm a, Labeled a, Show (Ring a), Fractional (Ring a)) => Ring a -> Map.Map Int [a] -> String
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
