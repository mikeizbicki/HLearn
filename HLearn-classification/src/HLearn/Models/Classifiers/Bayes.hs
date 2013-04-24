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
{-# LANGUAGE DataKinds #-}

module HLearn.Models.Classifiers.Bayes
    where

import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data types

data Bayes (labelIndex::Nat) dist = Bayes
    { labelDist :: !(Margin (Nat1Box (ToNat1 labelIndex)) dist)
    , attrDist  :: !(MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    }
--     deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- Algebra

instance 
    ( Semigroup (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    , Semigroup (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    ) => Semigroup (Bayes labelIndex dist) 
        where
    b1 <> b2 = Bayes
        { labelDist = labelDist b1 <> labelDist b2
        , attrDist = attrDist b1 <> attrDist b2
        }

instance 
    ( Monoid (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    , Monoid (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    ) => Monoid (Bayes labelIndex dist) 
        where
    mempty = Bayes mempty mempty
    b1 `mappend` b2 = Bayes
        { labelDist = labelDist b1 `mappend` labelDist b2
        , attrDist = attrDist b1 `mappend` attrDist b2
        }

-------------------------------------------------------------------------------
-- Training

instance 
    ( Datapoint dist ~ Datapoint (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    , Monoid (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    , Monoid (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    , Semigroup (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    , Semigroup (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    , HomTrainer (MarginalizeOut (Nat1Box (ToNat1 labelIndex)) dist)
    ) => HomTrainer (Bayes labelIndex dist) 
        where
    type Datapoint (Bayes labelIndex dist) = Datapoint dist
    train1dp dp = Bayes
        { labelDist = mempty
        , attrDist = train1dp dp
        }

-------------------------------------------------------------------------------
-- Classification

instance Probabilistic (Bayes labelIndex dist) where
    type Probability (Bayes labelIndex dist) = Probability dist

instance 
    ( Mean (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    ) => Classifier (Bayes labelIndex dist) 
        where
    type Label (Bayes labelIndex dist) = Datapoint (Margin (Nat1Box (ToNat1 labelIndex)) dist)
    type UnlabeledDatapoint (Bayes labelIndex dist) = Datapoint dist
    type ResultDistribution (Bayes labelIndex dist) = Margin (Nat1Box (ToNat1 labelIndex)) dist
        
    probabilityClassify bayes dp = (pdf (labelDist bayes) label)*(pdf (attrDist bayes) dp)
    
--     probabilityClassify bayes dp = 
--         Categorical $ Map.mapWithKey (\label dist -> (pdf dist dp)*(pdf (labelDist bayes) label)) $ attrDist bayes

    
-------------------------------------------------------------------------------
-- Test

-- data Sex = Male | Female
--     deriving (Read,Show,Eq,Ord)
-- 
-- ds = 
--     [ (Male, (6::Double):::(180::Double):::(12::Double):::HNil)
--     , (Male, 5.92:::190:::11:::HNil)
--     , (Male, 5.58:::170:::12:::HNil)
--     , (Male, 5.92:::165:::10:::HNil)
--     , (Female, 5:::100:::6:::HNil)
--     , (Female, 5.5:::150:::8:::HNil)
--     , (Female, 5.42:::130:::7:::HNil)
--     , (Female, 5.75:::150:::9:::HNil)
--     ]
-- 
-- dp = (6:::130:::8:::HNil)::(HList '[Double,Double,Double])
-- 
-- model = train ds :: Bayes
--     (Multivariate (HList '[Double,Double,Double])
--        '[ Independent Normal '[Double]
-- --         , Independent Normal '[Double,Double]
--         , Dependent MultiNormal '[Double,Double]
--         ])
--     Sex
--     Double
-- 
-- dist = train (map snd ds) :: Multivariate (HList '[Double,Double,Double])
--    '[ Independent Normal '[Double]
--     , Dependent MultiNormal '[Double,Double]
--     ]
--     Double
    