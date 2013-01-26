{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module HLearn.Models.Ensemble
    where

import Data.Sequence hiding ((|>),(<|))
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Classification

-------------------------------------------------------------------------------
-- Ensemble

data Ensemble' modelparams model prob = Ensemble'
    { params :: modelparams
    , models :: Seq (prob,model)
    }

instance Triangle (Ensemble modelparams model prob) (prob,model) where
    wm <| (SGJust ens') = SGJust $ ens' { models = wm <| (models ens') }
    (SGJust ens') |> wm = SGJust $ ens' { models = (models ens') |> wm }

type Ensemble modelparams model prob = RegSG2Group (Ensemble' modelparams model prob) 

instance (Num prob, Ord label, ProbabilityClassifier model datatype label prob) => 
    ProbabilityClassifier (Ensemble modelparams model prob) datatype label prob
        where
              
    probabilityClassify (SGJust ens') dp = {-foldl (\(w1,m1) m2 -> (w1.*m1) <> m2) mempty $-} 
        reduce $ F.toList $ fmap (\(w,m) -> w.* (probabilityClassify m dp)) (models ens')

-------------------------------------------------------------------------------
-- EnsembleAppender

{-newtype EnsembleAppender' modelparams model = EnsembleAppender' (Ensemble' modelparams model)

instance Model modelparams (EnsembleAppender' modelparams model) where
    getparams (EnsembleAppender' ens) = getparams ens

instance Semigroup (EnsembleAppender' modelparams model) where-}
--     ens1 <> ens2 = 