{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Models
    ( Labeled
    , Weighted
    , ModelParams (..)
    , Model (..)
    , Label (..)
--     , Trainer (..)
    , module Control.DeepSeq
    , module Data.Hashable
    , module Data.Binary
    )
    where
          
import Control.DeepSeq
import Data.Hashable
import Data.Binary

-------------------------------------------------------------------------------
-- Idioms

type Labeled var label  = (label,var)
type Weighted var       = (var,Double)

-- | I only ever expect labels of type Bool, Int, and String, but it may be convenient to use other types as well for something.  This class and instance exist so that we have some reasonable assumptions about what properties labels should have for our other classes to work with.  It also keeps us from writing so many constraints.
class (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

instance (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label


-------------------------------------------------------------------------------
-- ModelParams

class (NFData modelparams) => ModelParams modelparams

-- instance (ModelParams modelparams) => ModelParams (HLearn modelparams)

-------------------------------------------------------------------------------
-- Model

class (ModelParams modelparams, NFData model) => Model modelparams model | modelparams -> model, model -> modelparams where
    params :: model -> modelparams

-- instance (Model modelparams model) => Model (HLearn modelparams) model where
--     params model = return $ params model
-- 
-- instance (Model modelparams model) => Model (HLearn modelparams) (HLearn model) where
--     params model = liftM params model



{-instance (Trainer modelparams datatype model) => Trainer (HLearn modelparams) datatype model where
    train modelparams datatype = liftM2 train modelparams (return datatype)-}
    