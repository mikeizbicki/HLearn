{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module HLearn.Models.Ensemble.Common
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- Ensemble

data Ensemble' modelparams weight model = Ensemble'
    { params :: modelparams
    , models :: Seq (weight,model)
    }

instance Model modelparams (Ensemble' modelparams weight model) where
    getparams = params

instance Triangle (Ensemble' modelparams weight) where
    model <| ens = ens { models = model <| (models ens) }
    ens |> model = ens { models = (models ens) |> model }

type Ensemble modelparams model = RegSG2Group (Ensemble' modelparams weight model)

-------------------------------------------------------------------------------
-- EnsembleAppender

newtype EnsembleAppender' modelparams model = EnsembleAppender' (Ensemble' modelparams model)

instance Model modelparams (EnsembleAppender' modelparams model) where
    getparams (EnsembleAppender' ens) = getparams ens

instance Semigroup (EnsembleAppender' modelparams model) where
--     ens1 <> ens2 = 