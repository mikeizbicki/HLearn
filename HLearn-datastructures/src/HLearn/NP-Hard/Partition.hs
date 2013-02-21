{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- data types    

data Partition a (n::Nat) = Partition
    { bst :: !(BST a)
    , sets :: Map.Map Int [a]
    }
    deriving (Read,Show,Eq,Ord)

bst2partition :: forall a r n. (Norm a r, SingI n) => BST a -> Partition a n
bst2partition bst = Partition
    { bst = bst
    , sets = bst2sets (fromIntegral $ fromSing (sing :: Sing n)) bst
    }
    
bst2sets :: (Norm a r) => Int -> BST a -> Map.Map Int [a]
bst2sets n bst = Map.map cs2list $ F.foldr go base bst
    where
        base = Map.fromList [(i,mempty) | i<-[0..n-1]]
        go a sets = Map.insertWith (<>) minindex (cseq_singleton a) sets
            where
                minindex = fst $ argmin (\(k,p) -> total p) $ Map.toList sets

maxpartition :: (Norm a r) => Partition a n -> r
maxpartition p = maximum $ map magnitude $ Map.elems $ sets p

minpartition :: (Norm a r) => Partition a n -> r
minpartition p = minimum $ map magnitude $ Map.elems $ sets p

spread :: (Norm a r) => Partition a n -> r
spread p = (maxpartition p)-(minpartition p)

-------------------------------------------------------------------------------
-- Algebra

instance (Norm a r, SingI n) => Semigroup (Partition a n) where
    p1 <> p2 = bst2partition $ (bst p1) <> (bst p2)

instance (Norm a r, SingI n) => Monoid (Partition a n) where
    mempty = bst2partition mempty
    mappend = (<>)

-------------------------------------------------------------------------------
-- Training

instance Model (NoParams (Partition a n)) (Partition a n) where
    getparams _ = NoParams
    
instance (Norm a r, SingI n) => DefaultModel (NoParams (Partition a n)) (Partition a n) where
    defparams = NoParams
    
instance (Norm a r, SingI n) => HomTrainer (NoParams (Partition a n)) a (Partition a n) where
    train1dp' _ dp = bst2partition $ train1dp dp
    
-------------------------------------------------------------------------------
-- CountingSeq

data CountingSeq r a = CountingSeq 
    { total :: r
    , countingseq :: Seq.Seq a
    }
    deriving (Read,Show,Eq,Ord)
    
cs2list :: CountingSeq r a -> [a]
cs2list = F.toList . countingseq
    
cseq_singleton :: (Norm a r) => a -> CountingSeq r a
cseq_singleton a = CountingSeq (magnitude a) $ Seq.singleton a
    
instance F.Foldable (CountingSeq r) where
    foldr f b (CountingSeq t s) = F.foldr f b s
    
instance (Num r, Norm a r) => Triangle (CountingSeq r a) a where
    (CountingSeq t s) |> a = CountingSeq (t+(magnitude a)) (s|>a)
    a <| (CountingSeq t s) = CountingSeq (t+(magnitude a)) (a<|s)
    
instance (Num r) => Semigroup (CountingSeq r a) where
    (CountingSeq t1 s1)<>(CountingSeq t2 s2)=CountingSeq (t1+t2) (s1<>s2)
    
instance (Num r) => Monoid (CountingSeq r a) where
    mempty = CountingSeq 0 mempty
    mappend = (<>)
