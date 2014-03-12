module HLearn.Models.Classifiers.Centroid
    where

import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data structures

data Centroid vector = Centroid
    { c_numdp :: Scalar vector
    , vector :: vector
    }

deriving instance (Show (Scalar vector), Show vector) => Show (Centroid vector)
deriving instance (Read (Scalar vector), Read vector) => Read (Centroid vector)
deriving instance (Eq   (Scalar vector), Eq   vector) => Eq   (Centroid vector)
deriving instance (Ord  (Scalar vector), Ord  vector) => Ord  (Centroid vector)

-------------------------------------------------------------------------------
-- algebra

instance (Num (Scalar vector), Monoid vector) => Monoid (Centroid vector) where
    mempty = Centroid 0 mempty
    c1 `mappend` c2 = Centroid
        { c_numdp = c_numdp c1 + c_numdp c2
        , vector = vector c1 <> vector c2
        }

type instance Scalar (Centroid vector) = Scalar vector

instance 
    ( MetricSpace vector
    , VectorSpace vector
    ) => MetricSpace (Centroid vector)
        where
    distance v1 v2 = distance (vector v1 /. c_numdp v1) (vector v2 /. c_numdp v2)
    
-------------------------------------------------------------------------------
-- model

instance NumDP (Centroid vector) where
    numdp = c_numdp

instance 
    ( Monoid vector
    , Num (Scalar vector)
    ) => HomTrainer (Centroid vector) 
        where
    type Datapoint (Centroid vector) = vector
    
    train1dp dp = Centroid { c_numdp=1, vector=dp }
