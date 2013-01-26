{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module HLearn.Models.Classifiers.Bayes
    where

import Unsafe.Coerce
import qualified Data.Map as Map

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification


-------------------------------------------------------------------------------
-- Bayes

data BayesParams distparams = BayesParams distparams
    deriving (Read,Show,Eq,Ord)

data Bayes' dist label prob = Bayes'
    { labelDist :: !(Categorical label prob)
    , attrDist  :: !(Map.Map label (dist prob))
    }
    deriving (Read,Show,Eq,Ord)
    
type Bayes dist label prob = RegSG2Group (Bayes' dist label prob)

instance (Model distparams (dist prob)) => Model (BayesParams distparams) (Bayes dist label prob) where
    getparams (SGJust b) = --error "Bayes.getparams undefined"
        BayesParams $ getparams $ snd $  head $ Map.toList $ attrDist $ b
    
instance (DefaultModel distparams (dist prob)) => DefaultModel (BayesParams distparams) (Bayes dist label prob) where
    defparams = BayesParams defparams

-------------------------------------------------------------------------------
-- Algebra

instance 
    ( Ord label, Num prob
    , Semigroup (dist prob)
--     , Model (BayesParams distparams) (Bayes dist label prob)
    ) => Semigroup (Bayes' dist label prob) 
        where
    b1 <> b2 = if False -- (getparams $ SGJust b1) /= (getparams $ SGJust b2)
        then error "Bayes.(<>): different params"
        else Bayes'
            { labelDist = labelDist b1 <> labelDist b2
            , attrDist  = Map.unionWith (<>) (attrDist b1) (attrDist b2)
            }

instance (Semigroup (Bayes' dist label prob)) => Abelian (Bayes' dist label prob)

instance (Ord label, RegularSemigroup (dist prob){-, Model distparams (dist prob)-}, Num prob) => RegularSemigroup (Bayes' dist label prob) where
    inverse b = b
        { labelDist = inverse $ labelDist b
        , attrDist  = Map.map inverse $ attrDist b
        }
    
instance 
    ( LeftOperator ring (dist prob)
    , LeftOperator ring (Categorical label prob)
    , RegularSemigroup (dist prob)
    , Ord label, Num prob
    ) => LeftOperator ring (Bayes' dist label prob) 
        where
    r .* b = b 
        { labelDist = r .* (labelDist b) 
        , attrDist  = Map.map (r .*) (attrDist  b)
        }
    
-- instance 
--     ( RightOperator ring distL
--     , RightOperator ring (Categorical label prob)
--     , Ord label, RegularSemigroup distL, Eq distparams, Num prob
--     ) => RightOperator ring (Bayes' distparams distL label prob) 
--         where
--     b *. r = b 
--         { labelDist = (labelDist b) *. r
--         , attrDist  = Map.map (*. r) (attrDist  b)
--         }
--     
-------------------------------------------------------------------------------
-- Training

instance 
    ( Semigroup (dist prob)
    , Ord label, Num prob, Eq prob
--     , Model (BayesParams distparams) (Bayes dist label prob)
    , HomTrainer distparams attr (dist prob)
    ) => HomTrainer (BayesParams distparams) (label,attr) (Bayes dist label prob) 
        where
    
    train1dp' (BayesParams params) (label,attr) = 
        SGJust $ Bayes' (train1dp label) (Map.singleton label $ train1dp' params attr)

-------------------------------------------------------------------------------
-- Classification

instance 
    ( Ord label
    , Ord prob
    , Fractional prob
--     , Eq distparams
    , RegularSemigroup (dist prob)
    , Distribution (dist prob) attr prob
    ) => ProbabilityClassifier (Bayes dist label prob) attr label prob where
    probabilityClassify (SGJust bayes) dp = 
        Categorical $ Map.mapWithKey (\label dist -> (pdf dist dp)*(pdf (labelDist bayes) label)) $ attrDist bayes
        

data Sex = Male | Female
    deriving (Read,Show,Eq,Ord)

ds = 
    [ (Male, (6::Double):::(180::Double):::(12::Double))
    , (Male, 5.92:::190:::11)
    , (Male, 5.58:::170:::12)
    , (Male, 5.92:::165:::10)
    , (Female, 5:::100:::6)
    , (Female, 5.5:::150:::8)
    , (Female, 5.42:::130:::7)
    , (Female, 5.75:::150:::9)
    ]

dp = (6:::130:::8)::(Double:::Double:::Double)

model = train ds :: Bayes
--     (Gaussian:::.Gaussian:::.Gaussian)
    (Moments:::.Moments:::.Moments)
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
    