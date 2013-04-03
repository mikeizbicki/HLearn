{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module HLearn.Models.Classifiers.Boosting.FixedBoost
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification

-------------------------------------------------------------------------------
-- data structures

newtype HBoostParams basemodel = HBoostParams
    { baseModels :: V.Vector basemodel }
    deriving (Read,Show,Eq,Ord)

data HBoost' weight basemodel = HBoost'
    { hboostparams :: HBoostParams basemodel 
    , weights :: V.Vector weight
    }
    deriving (Read,Show,Eq,Ord)

type HBoost weight basemodel = RegSG2Group (HBoost' weight basemodel)

-------------------------------------------------------------------------------
-- algebra

instance (Eq basemodel, Num weight) => Semigroup (HBoost' weight basemodel) where
    b1 <> b2 = if hboostparams b1/=hboostparams b2
        then error "HBoost'.(<>): params not equal"
        else b1 { weights = V.zipWith (+) (weights b1) (weights b2) }

instance (Eq basemodel, Num weight) => RegularSemigroup (HBoost' weight basemodel) where
    inverse b = b { weights = V.map negate (weights b) }

-------------------------------------------------------------------------------
-- model
    
instance (Eq basemodel) => ModelParams (HBoostParams basemodel) (HBoost weight basemodel) where
    getparams (SGJust b) = hboostparams b
    
instance 
    ( {-HomTrainer baseparams dp basemodel
    , -}ProbabilityClassifier basemodel dp label weight
    , Eq basemodel
    , Num weight
    , Ord label
    , Fractional weight
    ) => HomTrainer (HBoostParams basemodel) (label,dp) (HBoost weight basemodel) 
        where
              
    train1dp' (HBoostParams modelV) (label,dp) = SGJust $ HBoost'
        { hboostparams = HBoostParams modelV
        , weights = V.generate (V.length modelV) (\i -> pdf (probabilityClassify (modelV V.! i) dp) label)
        }
    
-------------------------------------------------------------------------------
-- classification

instance 
    ( Ord weight
    , Num weight
    , Ord label
    , ProbabilityClassifier basemodel dp label weight
    ) => ProbabilityClassifier (HBoost weight basemodel) dp label weight 
        where
              
    probabilityClassify (SGJust (HBoost' (HBoostParams models) weights)) dp = 
        reduce $ V.zipWith (.*) weights (fmap (\model -> probabilityClassify model dp) models)