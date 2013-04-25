{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Models.Classifiers.Perceptron
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.NearestNeighbor

-------------------------------------------------------------------------------
-- data structures

data Perceptron label weight dp = Perceptron 
    { centroids :: Map.Map label (Centroid weight dp)
    }

data Centroid weight vector = Centroid
    { weight :: weight
    , vector :: vector
    }

-------------------------------------------------------------------------------
-- algebra

instance (Num weight, Num vector) => Monoid (Centroid weight vector) where
    mempty = Centroid 0 0
    c1 `mappend` c2 = Centroid
        { weight = weight c1 + weight c2
        , vector = vector c1 + vector c2
        }

instance 
    ( MetricSpace weight vector
    , VectorSpace weight vector
    ) => MetricSpace weight (Centroid weight vector)
        where
    distance v1 v2 = distance (vector v1 /. weight v1) (vector v2 /. weight v2)
    
---------------------------------------
        
instance (Num weight, Ord label, Num dp) => Monoid (Perceptron label weight dp) where
    mempty = Perceptron mempty
    p1 `mappend` p2 = Perceptron
        { centroids = Map.unionWith (<>) (centroids p1) (centroids p2)
        }
    
-------------------------------------------------------------------------------
-- model

instance 
    ( Num weight
    , Num vector
    ) => HomTrainer (Centroid weight vector) 
        where
    type Datapoint (Centroid weight vector) = vector
    
    train1dp dp = Centroid { weight=1, vector=dp }
    
---------------------------------------

instance 
    ( Num weight
    , Ord label
    , Num dp
    ) => HomTrainer (Perceptron label weight dp) 
        where
    type Datapoint (Perceptron label weight dp) = (label,dp)
              
    train1dp (label,dp) = Perceptron $ Map.singleton label $ train1dp dp
    
-------------------------------------------------------------------------------
-- classification

instance Probabilistic (Perceptron label prob dp) where
    type Probability (Perceptron label prob dp) = prob

instance 
    ( Ord prob
    , Ord label
    , Num dp
    , Num prob
    , MetricSpace prob (Centroid prob dp)
    , prob ~ Double
    ) => Classifier (Perceptron label prob dp)
        where
    type Label (Perceptron label prob dp) = label
    type UnlabeledDatapoint (Perceptron label prob dp) = dp  
    type ResultDistribution (Perceptron label prob dp) = (Categorical label prob)
              
    probabilityClassify model dp = probabilityClassify nn (train1dp dp :: Centroid prob dp)
        where
            nn = NaiveNN $ Map.toList $ centroids model