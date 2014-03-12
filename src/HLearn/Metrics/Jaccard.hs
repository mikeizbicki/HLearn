module HLearn.Metrics.Jaccard
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype Jaccard a = Jaccard a
    deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- metric space

type instance Scalar (Jaccard a) = Scalar a 

instance 
    ( Norm (a s)
    , RealFrac (Scalar (a s))
    , Topology a
    , TopologyConstraint a s 
    ) => MetricSpace (Jaccard (a s)) 
        where
    distance (Jaccard xs) (Jaccard ys) = 1 - magnitude (intersection xs ys) / magnitude (union xs ys)

