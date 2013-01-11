{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module HLearn.Models.Classifiers.Bayes
    where

import qualified Data.Map as Map

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification

-------------------------------------------------------------------------------
-- Bayes

data BayesParams distparams = BayesParams (MultivariateParams distparams)
    deriving (Read,Show,Eq,Ord)

data Bayes' distparams distL label prob = Bayes'
    { params    :: BayesParams distparams
    , labelDist :: !(Categorical label prob)
    , attrDist  :: !(Map.Map label (Multivariate distparams distL))
    }
    deriving (Read,Show,Eq,Ord)
    
type Bayes distparams distL label prob = RegSG2Group (Bayes' distparams distL label prob)

instance 
    (Ord label, Eq distparams, RegularSemigroup distL, Num prob
    ) => Model (BayesParams distparams) (Bayes distparams distL label prob) where
    getparams (SGJust bayes) = params bayes
    
instance 
    (Ord label, Eq distparams, RegularSemigroup distL, Num prob
    , DefaultModel (MultivariateParams distparams) (Multivariate distparams distL)
    ) => DefaultModel (BayesParams distparams) (Bayes distparams distL label prob) where
    defparams = BayesParams defparams

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, RegularSemigroup distL, Eq distparams, Num prob) => Semigroup (Bayes' distparams distL label prob) where
    b1 <> b2 = if (params b1) /= (params b2)
        then error "Bayes.(<>): different params"
        else b1
            { labelDist = (labelDist b1) <> (labelDist b2) 
            , attrDist = Map.unionWith (<>) (attrDist b1) (attrDist b2)
            }

instance (Ord label, RegularSemigroup distL, Eq distparams, Num prob) => RegularSemigroup (Bayes' distparams distL label prob) where
    inverse b = b
        { labelDist = inverse $ labelDist b
        , attrDist  = Map.map inverse $ attrDist b
        }
    
-------------------------------------------------------------------------------
-- Training

instance 
    ( Ord label
    , Eq distparams
    , Num prob
    , RegularSemigroup distL
    , HomTrainer (MultivariateParams distparams) attr (Multivariate distparams distL)
    ) => HomTrainer (BayesParams distparams) (label,attr) (Bayes distparams distL label prob) 
        where
    
    train1dp' (BayesParams params) (label,attr) = 
        SGJust $ Bayes' (BayesParams params) (train1dp label) (Map.singleton label $ train1dp' params attr)

-------------------------------------------------------------------------------
-- Classification

instance 
    ( Ord label
    , Ord prob
    , Num prob
    , Eq distparams
    , RegularSemigroup distL
    , Distribution (Multivariate distparams distL) attr prob
    ) => ProbabilityClassifier (Bayes distparams distL label prob) attr label prob where
    probabilityClassify (SGJust bayes) dp = 
        Categorical $ Map.mapWithKey (\label dist -> pdf dist dp) $ attrDist bayes
   

data Sex = Male | Female
    deriving (Read,Show,Eq,Ord)

ds = 
    [ (Male, (6::Double):::(180::Double):::(12::Double))
    , (Male, 5.98:::190:::11)
    , (Male, 5.58:::170:::12)
    , (Male, 5.92:::165:::10)
    , (Female, 5:::100:::6)
    , (Female, 5.5:::150:::8)
    , (Female, 5.42:::130:::7)
    , (Female, 5.75:::150:::9)
    ]

model = train ds :: Bayes
    (GaussianParams Double:::GaussianParams Double:::GaussianParams Double)
    (Gaussian Double:::Gaussian Double:::Gaussian Double)
    Sex
    Double

{-ds = 
    [ ("animal","dog":::"food")
    , ("animal","dog":::"water")
    , ("plant","tree":::"water")
    ]

ds2 = 
    [ "dog":::"food"
    , "dog":::"water"
    , "tree":::"water"
    ]
    
m = train ds :: Bayes
    (CategoricalParams ::: CategoricalParams)
    (Categorical String Double ::: Categorical String Double)
    String
    Double

m2 = train ds2 :: Multivariate
    (CategoricalParams ::: CategoricalParams)
    (Categorical String Double ::: Categorical String Double)-}
    