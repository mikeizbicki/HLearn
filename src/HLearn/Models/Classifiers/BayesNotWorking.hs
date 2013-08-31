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
{-# LANGUAGE TemplateHaskell #-}

module HLearn.Models.Classifiers.Bayes
    where


import Debug.Trace
import qualified Data.Map as Map
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data types

data Bayes labelLens dist = Bayes
    { labelDist :: !(Margin labelLens dist)
    , attrDist  :: !(MarginalizeOut labelLens dist)
    }
    
instance (Show (Margin labelLens dist), Show (MarginalizeOut labelLens dist)) => Show (Bayes labelLens dist)
--     deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- Algebra

-- instance 
--     ( {-Monoid (Margin labelLens dist)
--     , Monoid (MarginalizeOut labelLens dist)
--     -}) => Monoid (Bayes labelLens dist) 
--         where
--     mempty = undefined -- Bayes undefined undefined -- mempty mempty
--     b1 `mappend` b2 = undefined {-Bayes
--         { labelDist = undefined -- labelDist b1 `mappend` labelDist b2
--         , attrDist = undefined -- attrDist b1 `mappend` attrDist b2
--         }-}

-------------------------------------------------------------------------------
-- Training

-- instance 
--     ( {-Datapoint dist ~ Datapoint (MarginalizeOut labelLens dist)
--     , TypeFunction labelLens
--     , Datapoint (Margin labelLens dist) ~ Range labelLens
--     , Datapoint (MarginalizeOut labelLens dist) ~ Domain labelLens
--     , Monoid (MarginalizeOut labelLens dist)
--     , Monoid (Margin labelLens dist)
--     , HomTrainer (MarginalizeOut labelLens dist)
--     , HomTrainer (Margin labelLens dist)
--     -}) => HomTrainer (Bayes labelLens dist) 
--         where
--     type Datapoint (Bayes labelLens dist) = Datapoint dist
--     train = undefined
--     train1dp dp = trace "train1dp" $ Bayes
--         { labelDist = undefined -- train1dp $ typefunc (undefined::labelLens) dp
--         , attrDist = undefined -- train1dp dp
--         }

-------------------------------------------------------------------------------
-- Classification

instance Probabilistic (Bayes labelIndex dist) where
    type Probability (Bayes labelIndex dist) = Probability dist

instance 
    ( Mean (Margin labelLens dist)
    , Margin labelLens dist ~ Categorical label prob
    , Ord prob, Fractional prob
    , prob ~ Probability (MarginalizeOut labelLens dist)
    , Ord (Datapoint (Margin labelLens dist))
--     , PDF dist
--     , Ord (Datapoint (ResultDistribution (Margin labelLens dist)))
    , Datapoint dist ~ Datapoint (MarginalizeOut labelLens dist)
    , PDF (MarginalizeOut labelLens dist)
    ) => Classifier (Bayes labelLens dist) 
        where
    type Label (Bayes labelLens dist) = Datapoint (Margin labelLens dist)
    type UnlabeledDatapoint (Bayes labelLens dist) = Datapoint dist
    type ResultDistribution (Bayes labelLens dist) = Margin labelLens dist
        
--     probabilityClassify bayes dp = (pdf (labelDist bayes) label)*(pdf (attrDist bayes) dp)
    
    probabilityClassify bayes dp = undefined -- Categorical $ Map.fromList $ map (\k -> (k,prob k)) labelL
        where
            prob k = pdf (labelDist bayes) k * pdf (attrDist bayes) dp
            
            Categorical labelMap = labelDist bayes
            labelL = Map.keys labelMap
--     probabilityClassify bayes dp = 
--         Categorical $ Map.mapWithKey (\label dist -> (pdf dist dp)*(pdf (labelDist bayes) label)) $ attrDist bayes

    
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

dp = (6:::130:::8:::HNil)::(HList '[Double,Double,Double])

-- model = train ds :: BC

type BC = Bayes TH_sex
    (Multivariate Human
       '[ MultiCategorical '[Sex]
        , Independent Normal '[Double,Double,Double]
        ] Double)

instance Monoid (Bayes label dist)

{-dist = train (map snd ds) :: Multivariate (HList '[Double,Double,Double])
   '[ Independent Normal '[Double]
    , Dependent MultiNormal '[Double,Double]
    ]
    Double-}
    