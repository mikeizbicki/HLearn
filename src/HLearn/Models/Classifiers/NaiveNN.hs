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

newtype NaiveNN container label dp = NaiveNN
    { getcontainer :: container (label,dp) }
-- deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
    
deriving instance (Show (container (label,dp))) => Show (NaiveNN container label dp)
    
-------------------------------------------------------------------------------
-- algebra
    
instance (Monoid (container (label,dp))) => Monoid (NaiveNN container label dp) where
    mempty = NaiveNN mempty
    mappend nn1 nn2 = NaiveNN $ getcontainer nn1 `mappend` getcontainer nn2

-------------------------------------------------------------------------------
-- model

instance
    ( Applicative container
    , Monoid (container (label,dp))
    ) => HomTrainer (NaiveNN container label dp)
        where
    type Datapoint (NaiveNN container label dp) = (label,dp)
    train1dp ldp = NaiveNN $ pure ldp
    
-------------------------------------------------------------------------------
-- classification

instance (Probabilistic (NaiveNN container label dp)) where
    type Probability (NaiveNN container label dp) = Double

neighborList ::
    ( F.Foldable container
    , MetricSpace dp
    , Ord (Ring dp)
    ) => dp -> NaiveNN container label dp -> [(label,dp)]
neighborList dp (NaiveNN dps) = sortBy f $ F.toList dps
    where
        f (_,dp1) (_,dp2) = compare (distance dp dp1) (distance dp dp2)


instance
    ( Ord label
    , label ~ Label (label,dp)
    , F.Foldable container
    , MetricSpace dp
    , Ord (Ring dp)
    , label ~ Ring (dp)
    ) => ProbabilityClassifier (NaiveNN container label dp)
        where
    type ResultDistribution (NaiveNN container label dp) = Categorical (Ring dp) label
              
    probabilityClassify nn dp = train (map fst $ take k $ neighborList dp nn)
        where
            k = 1
