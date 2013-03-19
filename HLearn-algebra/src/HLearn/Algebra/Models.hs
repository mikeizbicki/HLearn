{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE OverlappingInstances #-}

-- | The 'HomTrainer' class forms the base of the HLearn library.  It represents homomorphisms from a free monoid/group to any other structure.  This captures our intuitive notion of how data mining and machine learning algorithms should behave, but in a way that allows for the easy creation of parallel and online algorithms.
--
-- Unfortunately, we must slightly complicate the matter by also introducing the 'ModelParams' class.  Many learning algorithms take some sort of parameters, and we need the model class to define what those parameters should look like.  

module HLearn.Algebra.Models
    ( 
    -- * Parameters
    ModelParams (..)
    , DefaultParams (..)
    , NoParams (..)
    
    -- * HomTrainer
    , HomTrainer (..)
    , DefaultHomTrainer (..)
    
    -- * Convenience functions
    , sub1dp
    , subBatch
    
    , Weighted (..)
    -- * Type synonyms
--     , Labeled
--     , Weighted
--     , Label (..)
    )
    where
          
import qualified Control.ConstraintKinds as CK
import Data.Foldable
          
import HLearn.Algebra.Functions
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-- import Control.DeepSeq
-- import Data.Hashable
-- import Data.Binary

-------------------------------------------------------------------------------
-- ModelParams

-- | Every model needs a parameters structure.  If our model doesn't require parameters, we can use this wrapper to make that explicit.
data NoParams model = NoParams
    deriving (Read,Show,Eq,Ord)

-- | current FunctionalDependencies disallow the following:
-- instance (ModelParams (NoParams model) model) => DefaultParams (NoParams model) model where
--     defparams = NoParams

-- | Every model has at least one data type that that fully describes its parameters.  Many models do not actually *need* any parameters, in which case they will simply use an empty data type for modelparams.
class (Eq modelparams) => ModelParams modelparams model | modelparams -> model, model -> modelparams where
    getparams :: model -> modelparams
    
-- | For those algorithms that do not require parameters (or that have reasonable default parameters), this class lets us use a more convenient calling notation.
class (ModelParams modelparams model) => DefaultParams modelparams model | model -> modelparams, modelparams -> model where
    defparams :: modelparams

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class 
    ( Semigroup model
    , Monoid model
    , ModelParams modelparams model
    ) => HomTrainer modelparams datapoint model | model -> modelparams, model -> datapoint, modelparams -> model
        where

    -- | The singleton trainer
    {-# INLINE train1dp' #-}
    train1dp' :: modelparams -> datapoint -> model
    train1dp' modelparams = unbatch (train' modelparams)
    
    -- | The batch trainer
    {-# INLINE train' #-}
    train' ::     
        ( Functor container
        , Foldable container
        ) => modelparams -> container datapoint -> model
--     train' modelparams = addBatch (train1dp' modelparams)
    train' modelparams = batch (train1dp' modelparams)

    -- | The online trainer
    {-# INLINE add1dp #-}
    add1dp :: model -> datapoint -> model
--     add1dp = online $ unbatch $ offline addBatch
    add1dp model = online (train1dp' (getparams model :: modelparams)) model
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    addBatch ::
        ( Functor container
        , Foldable container
        ) =>  model -> container datapoint -> model
--     addBatch model = online (batch $ train1dp' $ getparams model) model
    addBatch model = online (train' $ getparams model) model

    -- | CK methods take advantage of the ContraintKinds extension to allow containers that require constraints.  In particular, they allow the use of Unboxed Vectors, which can improve performance.
    {-# INLINE trainCK' #-}
    trainCK' ::     
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container datapoint
        ) => modelparams -> container datapoint -> model
    trainCK' modelparams = batchCK (train1dp' modelparams)

    {-# INLINE addBatchCK #-}
    addBatchCK ::
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container datapoint
--         Foldable container
--         , Functor container
        ) =>  model -> container datapoint -> model
    addBatchCK model = online (trainCK' (getparams model :: modelparams)) model
    

sub1dp :: 
    ( RegularSemigroup model
    , HomTrainer modelparams datapoint model
    , DefaultParams modelparams model
    ) => model -> datapoint -> model
sub1dp model dp = model <> (inverse $ train1dp dp)

-- model -. dp = sub1dp model dp
-- model -. xs = subBatch model xs

subBatch :: 
    ( Functor container
    , Foldable container
    , RegularSemigroup model
    , HomTrainer modelparams datapoint model
    , DefaultParams modelparams model
    ) => model -> container datapoint -> model
subBatch model xs = model <> (inverse $ train xs)

-- instance 
--     ( HomTrainer modelparams datapoint model
--     , LeftOperator r model
--     ) => HomTrainer modelparams (r,datapoint) model where
--         train1dp' modelparams (r,dp) = r .* (train1dp' modelparams dp)

-- instance 
--     ( HomTrainer modelparams datapoint model
--     , RightOperator r model
--     ) => HomTrainer modelparams (datapoint,r) model where
--         train1dp' modelparams (dp,r) = (train1dp' modelparams dp) *. r


-- | Provides parameterless functions for those training algorithms that do not require parameters
class 
    ( DefaultParams modelparams model
    , HomTrainer modelparams datapoint model
    ) => DefaultHomTrainer modelparams datapoint model | model -> modelparams
        where
              
    -- | A singleton trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train1dp #-}
    train1dp :: datapoint -> model
    train1dp = train1dp' (defparams :: modelparams)
    
    -- | A batch trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train #-}
    train :: 
        ( Foldable container
        , Functor container
        ) => container datapoint -> model
    train = train' (defparams :: modelparams)

    {-# INLINE trainCK #-}
    trainCK :: 
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container datapoint
--         Foldable container
--         , Functor container
        ) => container datapoint -> model
    trainCK = trainCK' (defparams :: modelparams)

instance 
    ( DefaultParams modelparams model
    , HomTrainer modelparams datapoint model
    ) => DefaultHomTrainer modelparams datapoint model
    
    
-------------------------------------------------------------------------------

    
newtype Weighted model = Weighted { unweight :: model }
    deriving (Read,Show,Eq,Ord)

instance (Semigroup model) => Semigroup (Weighted model) where
    Weighted a <> Weighted b = Weighted $ a <> b
    
instance (Monoid model) => Monoid (Weighted model) where
    mempty = Weighted mempty
    Weighted a `mappend` Weighted b = Weighted $ a `mappend` b
    
instance (RegularSemigroup model) => RegularSemigroup (Weighted model) where
    inverse (Weighted a) = Weighted (inverse a)
    
instance (ModelParams modelparams model) => ModelParams (Weighted modelparams) (Weighted model) where
    getparams (Weighted model) = Weighted $ getparams model
    
instance (DefaultParams modelparams model) => DefaultParams (Weighted modelparams) (Weighted model) where
    defparams = Weighted $ defparams

instance 
    ( Module ring model
    , HomTrainer modelparams dp model
    ) => 
    HomTrainer (Weighted modelparams) (ring,dp) (Weighted model) 
        where
        
    train1dp' (Weighted params) (ring,dp) = Weighted $ ring .* (train1dp' params dp)