{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module HLearn.Models.Classifiers.Perceptron
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification
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

instance (Num weight, Num vector) => Semigroup (Centroid weight vector) where
    c1 <> c2 = Centroid
        { weight = weight c1 + weight c2
        , vector = vector c1 + vector c2
        }
        
instance (Num weight, Num vector) => Monoid (Centroid weight vector) where
    mempty = Centroid 0 0
    mappend = (<>)

instance 
    ( MetricSpace weight vector
    , VectorSpace weight vector
    ) => MetricSpace weight (Centroid weight vector)
        where
    dist v1 v2 = dist (vector v1 /. weight v1) (vector v2 /. weight v2)
    
---------------------------------------
        
instance (Num weight, Ord label, Num dp) => Semigroup (Perceptron label weight dp) where
    p1 <> p2 = Perceptron
        { centroids = Map.unionWith (<>) (centroids p1) (centroids p2)
        }

instance (Num weight, Ord label, Num dp) => Monoid (Perceptron label weight dp) where
    mempty = Perceptron mempty
    mappend = (<>)
    
-------------------------------------------------------------------------------
-- model

instance ModelParams (NoParams (Centroid weight vector)) (Centroid weight vector) where
    getparams _ = NoParams
    
instance DefaultParams (NoParams (Centroid weight vector)) (Centroid weight vector) where
    defparams = NoParams
    
instance 
    ( Num weight
    , Num vector
    ) => HomTrainer (NoParams (Centroid weight vector)) vector (Centroid weight vector) 
        where
              
    train1dp' _ dp = Centroid { weight=1, vector=dp }
    
---------------------------------------

instance ModelParams (NoParams (Perceptron label weight dp)) (Perceptron label weight dp) where
    getparams _ = NoParams
    
instance DefaultParams (NoParams (Perceptron label weight dp)) (Perceptron label weight dp) where
    defparams = NoParams
    
instance 
    ( Num weight
    , Ord label
    , Num dp
    ) => HomTrainer (NoParams (Perceptron label weight dp)) (label,dp) (Perceptron label weight dp) 
        where
              
    train1dp' _ (label,dp) = Perceptron $ Map.singleton label $ train1dp dp
    
-------------------------------------------------------------------------------
-- classification

instance 
    ( Ord prob
    , Ord label
    , Num dp
    , Num prob
    , MetricSpace prob (Centroid prob dp)
    ) => ProbabilityClassifier (Perceptron label prob dp) dp label prob 
        where
              
    probabilityClassify model dp = probabilityClassify nn (train1dp dp :: Centroid prob dp)
        where
            nn = NaiveNN $ Map.toList $ centroids model