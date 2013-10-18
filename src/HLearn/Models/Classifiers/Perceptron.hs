module HLearn.Models.Classifiers.Perceptron
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.Centroid
import HLearn.Models.Classifiers.NearestNeighbor

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
    , HasRing dp
    , Ord label
    ) => HomTrainer (Perceptron label dp) 
        where
    type Datapoint (Perceptron label dp) = (label,dp)
              
    train1dp (label,dp) = Perceptron $ Map.singleton label $ train1dp dp
    
-------------------------------------------------------------------------------
-- classification

instance (HasRing dp) => Probabilistic (Perceptron label dp) where
    type Probability (Perceptron label dp) = Ring dp

instance 
    ( Ord label
    , Ord (Ring dp)
    , MetricSpace (Centroid dp)
    , Monoid dp
    , HasRing dp
    ) => ProbabilityClassifier (Perceptron label dp)
        where
    type ResultDistribution (Perceptron label dp) = (Categorical (Ring dp) label)
              
    probabilityClassify model dp = probabilityClassify nn (train1dp (dp) :: Centroid dp)
        where
            nn = NaiveNN $ Map.toList $ centroids model

instance 
    ( ProbabilityClassifier (Perceptron label dp)
    , Ord dp
    , Ord (Ring dp)
    , Ord label
    , Num (Ring dp)
    ) => Classifier (Perceptron label dp)
        where
    classify model dp = mean $ probabilityClassify model dp
