module HLearn.Models.Classifiers.NaiveNN
    where

import Control.Applicative
import qualified Data.Foldable as F
import Data.List

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

newtype NaiveNN container dp = NaiveNN
    { getcontainer :: container dp }
-- deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
deriving instance (Show (container dp)) => Show (NaiveNN container dp)
    
-------------------------------------------------------------------------------
-- algebra
    
instance (Monoid (container dp)) => Monoid (NaiveNN container dp) where
    mempty = NaiveNN mempty
    mappend nn1 nn2 = NaiveNN $ getcontainer nn1 `mappend` getcontainer nn2

-------------------------------------------------------------------------------
-- model

instance
    ( Applicative container
    , Monoid (container ldp)
    ) => HomTrainer (NaiveNN container ldp)
        where
    type Datapoint (NaiveNN container ldp) = ldp
    train1dp ldp = NaiveNN $ pure ldp
    
-------------------------------------------------------------------------------
-- classification

instance (Probabilistic (NaiveNN container ldp)) where
    type Probability (NaiveNN container ldp) = Double

neighborList ::
    ( F.Foldable container
    , MetricSpace ldp
    , Ord (Ring ldp)
    ) => ldp -> NaiveNN container ldp -> [ldp]
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
    , Ord (Ring ldp)
    ) => ProbabilityClassifier (NaiveNN container ldp)
        where
    type ResultDistribution (NaiveNN container ldp) = Categorical (Ring (Attributes ldp)) (Label ldp)
              
    probabilityClassify nn dp = train (map getLabel $ take k $ neighborList (noLabel dp) nn)
        where
            k = 1

instance
    ( ProbabilityClassifier (NaiveNN container ldp)
    , label ~ Label ldp
    , dp ~ Attributes ldp
    , Ord (Ring dp)
    , MetricSpace dp
    , F.Foldable container
    , Ord label
    ) => Classifier (NaiveNN container ldp)
        where
    
    classify nn dp = mean $ probabilityClassify nn dp 
