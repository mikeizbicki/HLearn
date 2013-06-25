module HLearn.NPHard.BinPacking
    where
          
import qualified Data.Foldable as F
import qualified Data.Heap as Heap
import Data.List
import Data.List.Extras
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import GHC.TypeLits
import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.DataStructures.SortedVector

-------------------------------------------------------------------------------
-- data types    

data BinPacking (n::Nat) a = BinPacking
    { vector  :: !(SortedVector a)
    , packing :: Map.Map Int [a]
    }
    deriving (Read,Show,Eq,Ord)

bfd :: forall a n. (Norm a, Ord (Ring a), SingI n) => SortedVector a -> BinPacking n a
bfd vector = BinPacking
    { vector = vector
    , packing = vector2packing (fromIntegral $ fromSing (sing :: Sing n)) vector
    }

vector2packing :: (Norm a, Ord (Ring a)) => Ring a -> SortedVector a -> Map.Map Int [a]
vector2packing binsize vector = snd $ F.foldr cata (Map.empty,Map.empty) vector
    where
        cata x (weight2bin,packing) = case Map.lookupLE (binsize - magnitude x) weight2bin of
            Nothing -> (weight2bin',packing')
                where
                    newbin = Map.size packing + 1
                    weight2bin' = Map.insert (magnitude x) newbin weight2bin
                    packing' = Map.insert newbin [x] packing
            Just (weight,bin) -> (weight2bin', packing')
                where
                    weight2bin' = Map.insert (weight+magnitude x) bin $ 
                                  Map.delete weight weight2bin
                    packing' = Map.insertWith (++) bin [x] packing

-------------------------------------------------------------------------------
-- Algebra

instance (Ord a, Ord (Ring a), Norm a, SingI n) => Monoid (BinPacking n a) where
    mempty = bfd mempty
    p1 `mappend` p2 = bfd $ (vector p1) <> (vector p2)

---------------------------------------

instance CK.Functor (BinPacking n) where
    type FunctorConstraint (BinPacking n) x = (Ord x, Norm x, SingI n)
    fmap f sched = bfd $ CK.fmap f $ vector sched

-------------------------------------------------------------------------------
-- Training

instance (Ord a, Ord (Ring a), Norm a, SingI n) => HomTrainer (BinPacking n a) where
    type Datapoint (BinPacking n a) = a
    train1dp dp = bfd $ train1dp dp
    
