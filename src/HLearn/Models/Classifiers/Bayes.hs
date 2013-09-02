-- | Bayesian classification is one of the standard algorithms in machine learning.  Typically, we make the naive bayes assumption of assuming that none of our attributes are correlated.  The Bayes data type, however, is capable of both naive and non-naive assumptions.

module HLearn.Models.Classifiers.Bayes
    ( Bayes
    )
    where

import Debug.Trace
import qualified Data.Map as Map
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- dataDouble

newtype Bayes label dist = Bayes dist
    deriving (Read,Show,Eq,Ord,Monoid,Abelian,Group)

-------------------------------------------------------------------------------
-- Training

instance (Monoid dist, HomTrainer dist) => HomTrainer (Bayes label dist) where
    type Datapoint (Bayes label dist) = Datapoint dist
    train1dp dp = Bayes $ train1dp dp

-------------------------------------------------------------------------------
-- Classification

instance Probabilistic (Bayes labelLens dist) where
    type Probability (Bayes labelLens dist) = Probability dist

instance
    ( Margin labelLens dist ~ Categorical prob label
    , Ord label, Ord prob, Fractional prob
    , label ~ Label (Datapoint dist)
    , prob ~ Probability (MarginalizeOut labelLens dist)
    , Labeled (Datapoint dist)
    , Datapoint (MarginalizeOut labelLens dist) ~ Attributes (Datapoint dist)
    , PDF (MarginalizeOut labelLens dist)
    , PDF (Margin labelLens dist)
    , Marginalize labelLens dist
    ) => ProbabilityClassifier (Bayes labelLens dist) 
        where
    type ResultDistribution (Bayes labelLens dist) = Margin labelLens dist
    
--     probabilityClassify (Bayes dist) dp = undefined

    probabilityClassify (Bayes dist) dp = Categorical $ Map.fromList $ map (\k -> (k,prob k)) labelL
        where
            prob k = pdf labelDist k * pdf (attrDist k) dp
            
            labelDist = getMargin (undefined::labelLens) dist
            attrDist l = condition (undefined::labelLens) l dist
            
            Categorical labelMap = labelDist
            labelL = Map.keys labelMap

instance 
    ( ProbabilityClassifier (Bayes labelLens dist)
    , Label (Datapoint (Bayes labelLens dist)) ~ Datapoint (Margin labelLens dist)
    , Mean (Margin labelLens dist)
--      Labeled (Datapoint dist)
    ) => Classifier (Bayes labelLens dist)
        where
--     classify = undefined
    classify model dp = mean $ probabilityClassify model dp
