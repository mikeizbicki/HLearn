{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Models
    ( ModelParams (..)
    , Model (..)
    , Trainer (..)
    , module Control.DeepSeq
    )
    where
          
import Control.DeepSeq

-------------------------------------------------------------------------------
-- Idioms

type Labeled var label  = (label,var)
type Weighted var       = (var,Double)

-------------------------------------------------------------------------------
-- ModelParams

class (NFData modelparams) => ModelParams modelparams

-- instance (ModelParams modelparams) => ModelParams (HLearn modelparams)

-------------------------------------------------------------------------------
-- Model

class (ModelParams modelparams, NFData model) => Model modelparams model | modelparams -> model where
    params :: model -> modelparams

-- instance (Model modelparams model) => Model (HLearn modelparams) model where
--     params model = return $ params model
-- 
-- instance (Model modelparams model) => Model (HLearn modelparams) (HLearn model) where
--     params model = liftM params model

-------------------------------------------------------------------------------
-- Trainer

class {-(Model modelparams model) => -}Trainer modelparams datatype model | modelparams -> model where
    train :: modelparams -> datatype -> model

{-instance (Trainer modelparams datatype model) => Trainer (HLearn modelparams) datatype model where
    train modelparams datatype = liftM2 train modelparams (return datatype)-}
    