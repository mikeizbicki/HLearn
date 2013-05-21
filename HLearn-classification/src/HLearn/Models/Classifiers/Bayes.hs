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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
-- data types

newtype Bayes label dist = Bayes dist
    deriving (Read,Show,Eq,Ord,Monoid,Abelian,Group)

-------------------------------------------------------------------------------
-- Training

instance 
    ( Monoid dist
    , HomTrainer dist
    ) => HomTrainer (Bayes label dist) 
        where
    type Datapoint (Bayes label dist) = Datapoint dist
    train1dp dp = Bayes $ train1dp dp

-------------------------------------------------------------------------------
-- Classification

instance Probabilistic (Bayes labelLens dist) where
    type Probability (Bayes labelLens dist) = Probability dist

instance
    ( Margin labelLens dist ~ Categorical label prob
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
    
    probabilityClassify (Bayes dist) dp = Categorical $ Map.fromList $ map (\k -> (k,prob k)) labelL
        where
            prob k = pdf labelDist k * pdf (attrDist k) dp
            
            labelDist = getMargin (undefined::labelLens) dist
            attrDist l = condition (undefined::labelLens) l dist
            
            Categorical labelMap = labelDist
            labelL = Map.keys labelMap

-- instance 
--     ( --Datapoint (Margin labelLens dist) ~ Lab
--     ) => Classifier (Bayes labelLens dist)
--         where
--     type ResultDistribution (Bayes labelLens dist) = Margin labelLens dist
--               
--     probabilityClassify (Bayes dist) dp = undefined

-------------------------------------------------------------------------------
-- Test

data Sex = Male | Female
    deriving (Read,Show,Eq,Ord)

data Human = Human
    { _sex :: Sex
    , _weight :: Double
    , _height :: Double
    , _shoesize :: Double
    }
makeTypeLenses ''Human

instance Labeled Human where
    type Label Human = Sex
    type Attributes Human = Human
    
    getLabel h = _sex h
    getAttributes h = h
    
ds = 
    [ Human Male   6    180 12
    , Human Male   5.92 190 11
    , Human Male   5.58 170 12
    , Human Male   5.92 165 10
    , Human Female 5    100 6
    , Human Female 5.5  150 8
    , Human Female 5.42 130 7
    , Human Female 5.75 150 9
    ]

dp = Human Female 6 130 8
-- dp = (6:::130:::8:::HNil)::(HList '[Double,Double,Double])

model = train ds :: Bayes TH_sex MultiDist

type MultiDist = Multivariate Human
       '[ MultiCategorical '[Sex]
        , Independent Normal '[Double,Double,Double]
        ] Double

{-dist = train (map snd ds) :: Multivariate (HList '[Double,Double,Double])
   '[ Independent Normal '[Double]
    , Dependent MultiNormal '[Double,Double]
    ]
    Double-}
    