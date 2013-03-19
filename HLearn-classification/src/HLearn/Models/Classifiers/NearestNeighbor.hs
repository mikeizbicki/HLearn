{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Models.Classifiers.NearestNeighbor
    where

import Control.Applicative
import qualified Data.Foldable as F
import Data.List

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification

-------------------------------------------------------------------------------
-- data structures

newtype NaiveNN container label dp = NaiveNN
    { getcontainer :: container (label,dp) }
--     deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
-------------------------------------------------------------------------------
-- algebra

instance (Semigroup (container (label,dp))) => Semigroup (NaiveNN container label dp) where
    nn1 <> nn2 = NaiveNN $ getcontainer nn1 <> getcontainer nn2
    
instance (Monoid (container (label,dp))) => Monoid (NaiveNN container label dp) where
    mempty = NaiveNN mempty
    mappend nn1 nn2 = NaiveNN $ getcontainer nn1 `mappend` getcontainer nn2

-------------------------------------------------------------------------------
-- model

instance ModelParams (NoParams (NaiveNN container label dp)) (NaiveNN container label dp) where
    getparams _ = NoParams
    
instance DefaultParams (NoParams (NaiveNN container label dp)) (NaiveNN container label dp) where
    defparams = NoParams
    
instance 
    ( Applicative container
    , Monoid (container (label,dp))
    , Semigroup (container (label,dp))
    ) => HomTrainer (NoParams (NaiveNN container label dp)) (label,dp) (NaiveNN container label dp) where
    train1dp' _ ldp = NaiveNN $ pure ldp
    
-------------------------------------------------------------------------------
-- classification

neighborList :: 
    ( F.Foldable container
    , MetricSpace ring dp
    , Ord ring
    ) => dp -> NaiveNN container label dp -> [(label,dp)]
neighborList dp (NaiveNN dps) = sortBy f $ F.toList dps
    where
        f (_,dp1) (_,dp2) = compare (dist dp dp1) (dist dp dp2)


instance 
    ( Ord prob, Num prob, Ord label
    , F.Foldable container
    , MetricSpace prob dp
    ) => ProbabilityClassifier (NaiveNN container label dp) dp label prob 
        where
              
    probabilityClassify nn dp = unweight (train (map (\(l,dp) -> (1,l)) $ take k $ neighborList dp nn) :: Weighted (Categorical label prob))
        where
            k = 3