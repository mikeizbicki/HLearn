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
import HLearn.Algebra

-------------------------------------------------------------------------------
-- Ensemble

data Ensemble' modelparams model prob = Ensemble'
    { params :: modelparams
    , models :: Seq (prob,model)
    }

instance Triangle (Ensemble' modelparams model prob) (prob,model) where
    wm <| ens = ens { models = wm <| (models ens) }
    ens |> wm = ens { models = (models ens) |> wm }

type Ensemble modelparams model prob = RegSG2Group (Ensemble' modelparams model prob) 

-------------------------------------------------------------------------------
-- EnsembleAppender

{-newtype EnsembleAppender' modelparams model = EnsembleAppender' (Ensemble' modelparams model)

instance Model modelparams (EnsembleAppender' modelparams model) where
    getparams (EnsembleAppender' ens) = getparams ens

instance Semigroup (EnsembleAppender' modelparams model) where-}
--     ens1 <> ens2 = 