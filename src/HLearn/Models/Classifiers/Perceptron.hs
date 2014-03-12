module HLearn.Models.Classifiers.Perceptron
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.Centroid
import HLearn.Models.Classifiers.NaiveNN

-------------------------------------------------------------------------------
-- data structures

data Perceptron label dp = Perceptron 
    { centroids :: Map.Map label (Centroid dp)
    }
--     deriving (Read,Show,Eq,Ord)

deriving instance (Show (Centroid dp), Show label) => Show (Perceptron label dp)

-------------------------------------------------------------------------------
-- algebra

instance (Ord label, Monoid (Centroid dp)) => Monoid (Perceptron label dp) where
    mempty = Perceptron mempty
    p1 `mappend` p2 = Perceptron
        { centroids = Map.unionWith (<>) (centroids p1) (centroids p2)
        }
    
-------------------------------------------------------------------------------
-- model

instance 
    ( Monoid dp
    , Num (Scalar dp)
    , Ord label
    ) => HomTrainer (Perceptron label dp) 
        where
    type Datapoint (Perceptron label dp) = (label,dp)
              
    train1dp (label,dp) = Perceptron $ Map.singleton label $ train1dp dp
    
-------------------------------------------------------------------------------
-- classification

instance Probabilistic (Perceptron label dp) where
    type Probability (Perceptron label dp) = Scalar dp

-- instance 
--     ( Ord label
--     , Ord (Scalar dp)
--     , MetricSpace (Centroid dp)
--     , Monoid dp
--     , HasScalar dp
--     , label ~ Scalar dp
--     ) => ProbabilityClassifier (Perceptron label dp)
--         where
--     type ResultDistribution (Perceptron label dp) = (Categorical (Scalar dp) label)
--               
--     probabilityClassify model dp = probabilityClassify nn (train1dp (dp) :: Centroid dp)
--         where
--             nn = NaiveNN $ Map.toList $ centroids model
-- 
-- instance 
--     ( ProbabilityClassifier (Perceptron label dp)
--     , Ord dp
--     , Ord (Scalar dp)
--     , Ord label
--     , Num (Scalar dp)
--     ) => Classifier (Perceptron label dp)
--         where
--     classify model dp = mean $ probabilityClassify model dp
