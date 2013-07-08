module HLearn.DataStructures.KDSortedVector
    where

import qualified Data.Vector as V

import GHC.Prim
import GHC.TypeLits
import Unsafe.Coerce

import HLearn.Algebra hiding (Any)
import HLearn.DataStructures.SortedVector

-------------------------------------------------------------------------------
-- data types

class VectorSpaceDP dp where
    type Dimensions dp :: Nat
    type DimL dp :: [a]

type VectorL dp = HList (VectorL' dp (DimL dp))

type family VectorL' dp (xs::[a]) :: [a]
type instance VectorL' dp '[] = '[]
type instance VectorL' dp (x ': xs) = SortedVector (OrdWrapper x dp) ': (VectorL' dp xs)

newtype KDSortedVector dp = KDSortedVector
    { dimvecL :: VectorL dp
    }

deriving instance (Read (VectorL dp)) => Read (KDSortedVector dp)
deriving instance (Show (VectorL dp)) => Show (KDSortedVector dp)
deriving instance (Monoid (VectorL dp)) => Monoid (KDSortedVector dp)
deriving instance (Group (VectorL dp)) => Group (KDSortedVector dp)
deriving instance (Abelian (VectorL dp)) => Abelian (KDSortedVector dp)
deriving instance (Module (VectorL dp), HasRing dp) => Module (KDSortedVector dp)

newtype OrdWrapper dimindex dp = OrdWrapper dp
    deriving (Read,Show,Eq)

instance (VectorSpaceDimension dp dimindex result, Eq dp, Ord result) => Ord (OrdWrapper dimindex dp) where
    compare a b = compare (a ! dimindex) (b ! dimindex)
        where dimindex = undefined :: dimindex

-------------------------------------------------------------------------------
-- algebra

instance (HasRing dp) => HasRing (KDSortedVector dp) where
    type Ring (KDSortedVector dp) = Ring dp

-------------------------------------------------------------------------------
-- training

instance 
    ( Monoid (VectorL dp)
    , MakeHList dp (VectorL dp)
    ) => HomTrainer (KDSortedVector dp) 
        where
    type Datapoint (KDSortedVector dp) = dp
    train1dp dp = KDSortedVector $ makeHList dp 

---------------------------------------

class MakeHList a hlist where
    makeHList :: a -> hlist

instance MakeHList a (HList '[]) where
    makeHList a = HNil

instance (MakeHList dp (HList xs), HomTrainer x, Datapoint x ~ (OrdWrapper i dp)) => MakeHList dp (HList (x ': xs)) where
    makeHList dp = train1dp (OrdWrapper dp) ::: (makeHList dp :: HList xs)

-------------------------------------------------------------------------------
-- test

instance VectorSpaceDP (a,b) where
    type Dimensions (a,b) = 2
    type DimL (a,b) = [Sing 0,Sing 1]

instance (VectorSpaceDimension vectorspace dimindex result) => VectorSpaceDimension (OrdWrapper i vectorspace) dimindex result where
    (!) (OrdWrapper vectorspace) dimindex = vectorspace ! dimindex

class VectorSpaceDimension vectorspace dimindex result | vectorspace dimindex -> result where
    (!) :: vectorspace -> dimindex -> result

instance VectorSpaceDimension (a,b) (Sing 0) a where
    (!) (a,b) _ = a

instance VectorSpaceDimension (a,b) (Sing 1) b where
    (!) (a,b) _ = b

xs = [(0,0),(1,0),(2,-1),(-1,-2),(-2,3)] :: [(Double,Double)]
m = train xs :: KDSortedVector (Double,Double)
