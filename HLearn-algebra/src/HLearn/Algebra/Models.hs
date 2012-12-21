{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

-- | The 'HomTrainer' class forms the base of the HLearn library.  It represents homomorphisms from a free monoid/group to any other structure.  This captures our intuitive notion of how data mining and machine learning algorithms should behave, but in a way that allows for the easy creation of parallel and online algorithms.
--
-- Unfortunately, we must slightly complicate the matter by also introducing the 'Model' class.  Many learning algorithms take some sort of parameters, and we need the model class to define what those parameters should look like.  

module HLearn.Algebra.Models
    ( 
    -- * Parameters
    Model (..)
    , DefaultModel (..)
    
    -- * HomTrainer
    , HomTrainer (..)
    , DefaultHomTrainer (..)
    
    -- * Type synonyms
--     , Labeled
--     , Weighted
--     , Label (..)
    )
    where
          
import qualified Control.ConstraintKinds as CK
          
import HLearn.Algebra.Structures
import HLearn.Algebra.Functions

import Control.DeepSeq
-- import Data.Hashable
-- import Data.Binary

-------------------------------------------------------------------------------
-- Model

-- | Every model has at least one data type that that fully describes its parameters.  Many models do not actually *need* any parameters, in which case they will simply use an empty data type for modelparams.
class Model modelparams model | modelparams -> model{-, model -> modelparams-} where
    getparams :: model -> modelparams
    
-- | For those algorithms that do not require parameters (or that have reasonable default parameters), this class lets us use a more convenient calling notation.
class (Model modelparams model) => DefaultModel modelparams model where
    defparams :: modelparams

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class 
    ( Semigroup model
    , Monoid model
    , Model modelparams model
    ) => HomTrainer modelparams datapoint model
        where

    -- | The singleton trainer
    {-# INLINE train1dp' #-}
    train1dp' :: modelparams -> datapoint -> model
    train1dp' modelparams = unbatch (train' modelparams)
    
    -- | The batch trainer
    {-# INLINE train' #-}
    train' ::     
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        ) => modelparams -> container datapoint -> model
    train' modelparams = batch (train1dp' modelparams)
    
    -- | The online trainer
    {-# INLINE add1dp #-}
    add1dp :: model -> datapoint -> model
    add1dp model = online (train1dp' (getparams model :: modelparams)) model
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    {-# INLINE addBatch #-}
    addBatch ::
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        ) =>  model -> container datapoint -> model
    addBatch model = online (train' (getparams model :: modelparams)) model
    
    
-- | Provides parameterless functions for those training algorithms that do not require parameters
class 
    ( DefaultModel modelparams model
    , HomTrainer modelparams datapoint model
    ) => DefaultHomTrainer modelparams datapoint model
        where
              
    -- | A singleton trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train1dp #-}
    train1dp :: datapoint -> model
    train1dp = train1dp' (defparams :: modelparams)
    
    -- | A batch trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train #-}
    train :: 
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        ) => container datapoint -> model
    train = train' (defparams :: modelparams)
    
instance 
    ( DefaultModel modelparams model
    , HomTrainer modelparams datapoint model
    ) => DefaultHomTrainer modelparams datapoint model