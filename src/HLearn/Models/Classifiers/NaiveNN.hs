{-# LANGUAGE DataKinds #-}

module HLearn.Models.Classifiers.NaiveNN
    where

import Control.Applicative
import qualified Data.Foldable as F
import Data.List
import Data.Proxy

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

newtype NaiveNN (k::Nat) container dp = NaiveNN
    { getcontainer :: container dp }
-- deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
deriving instance (Show (container dp)) => Show (NaiveNN k container dp)
    
-------------------------------------------------------------------------------
-- algebra
    
instance (Monoid (container dp)) => Monoid (NaiveNN k container dp) where
    mempty = NaiveNN mempty
    mappend nn1 nn2 = NaiveNN $ getcontainer nn1 `mappend` getcontainer nn2

-------------------------------------------------------------------------------
-- model

instance
    ( Applicative container
    , Monoid (container ldp)
    ) => HomTrainer (NaiveNN k container ldp)
        where
    type Datapoint (NaiveNN k container ldp) = ldp
    train1dp ldp = NaiveNN $ pure ldp
    
-------------------------------------------------------------------------------
-- classification

instance (Probabilistic (NaiveNN k container ldp)) where
    type Probability (NaiveNN k container ldp) = Double

neighborList ::
    ( F.Foldable container
    , MetricSpace ldp
    , Ord (Scalar ldp)
    ) => ldp -> NaiveNN k container ldp -> [ldp]
neighborList dp (NaiveNN dps) = sortBy f $ F.toList dps
    where
--         f (_,dp1) (_,dp2) = compare (distance dp dp1) (distance dp dp2)
        f dp1 dp2 = compare (distance dp dp1) (distance dp dp2)


instance
    ( Ord label
    , ldp ~ MaybeLabeled maybelabel attr
    , Labeled ldp
    , label ~ Label ldp
    , F.Foldable container
    , MetricSpace ldp
    , Ord (Scalar ldp)
    , KnownNat k
    ) => ProbabilityClassifier (NaiveNN k container ldp)
        where
    type ResultDistribution (NaiveNN k container ldp) = Categorical (Scalar (Attributes ldp)) (Label ldp)
              
    probabilityClassify nn dp = train (map getLabel $ take k $ neighborList (noLabel dp) nn)
        where
            k = fromIntegral $ natVal (Proxy::Proxy k)

instance
    ( ProbabilityClassifier (NaiveNN k container ldp)
    , label ~ Label ldp
    , dp ~ Attributes ldp
    , Ord (Scalar dp)
    , MetricSpace dp
    , F.Foldable container
    , Ord label
    ) => Classifier (NaiveNN k container ldp)
        where
    
    classify nn dp = mean $ probabilityClassify nn dp 
