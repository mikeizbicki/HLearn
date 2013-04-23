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

import qualified Data.Map as Map

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data types

data Bayes dist label prob = Bayes
    { labelDist :: !(Categorical label prob)
    , attrDist  :: !(Map.Map label (dist prob))
    }
    deriving (Read,Show,Eq,Ord)

-------------------------------------------------------------------------------
-- Algebra

instance 
    ( Ord label, Num prob
    , Semigroup (dist prob)
    ) => Semigroup (Bayes dist label prob) 
        where
    b1 <> b2 = Bayes
            { labelDist = labelDist b1 <> labelDist b2
            , attrDist  = Map.unionWith (<>) (attrDist b1) (attrDist b2)
            }

instance (Semigroup (Bayes dist label prob)) => Abelian (Bayes dist label prob)

instance 
    ( Ord label
    , Num prob
    , Semigroup (dist prob)
    , Monoid (dist prob)
    ) => Monoid (Bayes dist label prob)
        where
    mempty = Bayes mempty mempty
    mappend = (<>)

instance (Ord label, RegularSemigroup (dist prob), Num prob) => RegularSemigroup (Bayes dist label prob) where
    inverse b = b
        { labelDist = inverse $ labelDist b
        , attrDist  = Map.map inverse $ attrDist b
        }
    
instance 
    ( LeftOperator ring (dist prob)
    , LeftOperator ring (Categorical label prob)
    , RegularSemigroup (dist prob)
    , Ord label, Num prob
    ) => LeftOperator ring (Bayes dist label prob) 
        where
    r .* b = b 
        { labelDist = r .* (labelDist b) 
        , attrDist  = Map.map (r .*) (attrDist  b)
        }
    
-- instance 
--     ( RightOperator ring distL
--     , RightOperator ring (Categorical label prob)
--     , Ord label, RegularSemigroup distL, Eq distparams, Num prob
--     ) => RightOperator ring (Bayes distparams distL label prob) 
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
    , HomTrainer (dist prob)
    ) => HomTrainer (Bayes dist label prob) 
        where
    type Datapoint (Bayes dist label prob) = (label,Datapoint (dist prob))
    
    train1dp (label,attr) = Bayes (train1dp label) (Map.singleton label $ train1dp attr)

-------------------------------------------------------------------------------
-- Classification

instance Probabilistic (Bayes dist label prob) where
    type Probability (Bayes dist label prob) = prob

instance 
    ( Ord label
    , Ord prob
    , Fractional prob
--     , Eq distparams
    , RegularSemigroup (dist prob)
    , Probabilistic (dist prob)
    , Probability (dist (Probability (Bayes dist label prob))) ~ Probability (Bayes dist label prob)
    , PDF (dist prob)
    ) => Classifier (Bayes dist label prob)
        where

    type Label (Bayes dist label prob) = label
    type UnlabeledDatapoint (Bayes dist label prob) = Datapoint (dist prob)
 
    probabilityClassify bayes dp = 
        Categorical $ Map.mapWithKey (\label dist -> (pdf dist dp)*(pdf (labelDist bayes) label)) $ attrDist bayes
        

data Sex = Male | Female
    deriving (Read,Show,Eq,Ord)

ds = 
    [ (Male, (6::Double):::(180::Double):::(12::Double):::HNil)
    , (Male, 5.92:::190:::11:::HNil)
    , (Male, 5.58:::170:::12:::HNil)
    , (Male, 5.92:::165:::10:::HNil)
    , (Female, 5:::100:::6:::HNil)
    , (Female, 5.5:::150:::8:::HNil)
    , (Female, 5.42:::130:::7:::HNil)
    , (Female, 5.75:::150:::9:::HNil)
    ]

dp = (6:::130:::8:::HNil)::(HList '[Double,Double,Double])

model = train ds :: Bayes
    (Multivariate (HList '[Double,Double,Double])
       '[ Independent Normal '[Double]
        , Dependent MultiNormal '[Double,Double]
        ])
    Sex
    Double

dist = train (map snd ds) :: Multivariate (HList '[Double,Double,Double])
   '[ Independent Normal '[Double]
    , Dependent MultiNormal '[Double,Double]
    ]
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
    