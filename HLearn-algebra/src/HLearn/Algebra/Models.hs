{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
    
    , Counter (..)
    -- * Convenience functions
--     , sub1dp
--     , subBatch
    
--     , Weighted (..)
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

-------------------------------------------------------------------------------
-- ModelParams

-- | Every model has at least one data type that that fully describes its parameters.  Many models do not actually *need* any parameters, in which case they will simply use an empty data type for modelparams.
class ModelParams model where
    type Params model
    getparams :: model -> Params model

-- | Every model needs a parameters structure.  If our model doesn't require parameters, we can use this wrapper to make that explicit.  This gives us access to the `DefaultHomTrainer` type class.
data NoParams = NoParams
    deriving (Read,Show,Eq,Ord)

class DefaultParams params where
    defparams :: params
    
instance DefaultParams NoParams where
    defparams = NoParams

-------------------------------------------------------------------------------
-- HomTrainer

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class (Semigroup model, Monoid model, ModelParams model) => HomTrainer model where
    type Datapoint model

    -- | The singleton trainer
    {-# INLINE train1dp' #-}
    train1dp' :: Params model -> Datapoint model -> model
    train1dp' modelparams = unbatch (train' modelparams)
    
    -- | The batch trainer
    {-# INLINE train' #-}
    train' ::     
        ( Functor container
        , Foldable container
        ) => Params model -> container (Datapoint model) -> model
    train' modelparams = batch (train1dp' modelparams)

    -- | The online trainer
    {-# INLINE add1dp #-}
    add1dp :: model -> Datapoint model -> model
    add1dp model = online (train1dp' (getparams model)) model
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    addBatch ::
        ( Functor container
        , Foldable container
        ) =>  model -> container (Datapoint model) -> model
    addBatch model = online (train' $ getparams model) model

    -- | CK methods take advantage of the ContraintKinds extension to allow containers that require constraints.  In particular, they allow the use of Unboxed Vectors, which can improve performance.
    {-# INLINE trainCK' #-}
    trainCK' ::     
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container (Datapoint model)
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) => Params model -> container (Datapoint model) -> model
    trainCK' modelparams = batchCK (train1dp' modelparams)

    {-# INLINE addBatchCK #-}
    addBatchCK ::
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container (Datapoint model)
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) =>  model -> container (Datapoint model) -> model
    addBatchCK model = online (trainCK' (getparams model)) model
    
-- | Provides parameterless functions for those training algorithms that do not require parameters
-- class ( HomTrainer model, Params model ~ NoParams) => DefaultHomTrainer model where
class ( HomTrainer model, DefaultParams (Params model)) => DefaultHomTrainer model where
              
    -- | A singleton trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train1dp #-}
    train1dp :: Datapoint model -> model
    train1dp = train1dp' defparams
    
    -- | A batch trainer that doesn't require parameters (uses 'defparams')
    {-# INLINE train #-}
    train :: 
        ( Foldable container
        , Functor container
        ) => container (Datapoint model) -> model
    train = train' defparams

    {-# INLINE trainCK #-}
    trainCK :: 
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container (Datapoint model)
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) => container (Datapoint model) -> model
    trainCK = trainCK' defparams

instance (HomTrainer model, DefaultParams (Params model)) => DefaultHomTrainer model    

-------------------------------------------------------------------------------
-- Counter instance for testing

data Counter sampletype = Counter {c::sampletype}
    deriving (Read,Show,Eq,Ord)
    
instance (Num sampletype) => Semigroup (Counter sampletype) where
    c1<>c2 = Counter $ c c1+c c2

instance (Num sampletype) => Monoid (Counter sampletype) where
    mempty = Counter 0
    mappend = (<>)

instance ModelParams (Counter sampletype) where
    type Params (Counter sampletype) = NoParams
    getparams _ = NoParams

instance (Num sampletype) => HomTrainer (Counter sampletype) where
    type Datapoint (Counter sampletype) = sampletype
    train1dp' _ dp = Counter 1

-------------------------------------------------------------------------------

    
-- newtype Weighted model = Weighted { unweight :: model }
--     deriving (Read,Show,Eq,Ord)
-- 
-- instance (Semigroup model) => Semigroup (Weighted model) where
--     Weighted a <> Weighted b = Weighted $ a <> b
--     
-- instance (Monoid model) => Monoid (Weighted model) where
--     mempty = Weighted mempty
--     Weighted a `mappend` Weighted b = Weighted $ a `mappend` b
--     
-- instance (RegularSemigroup model) => RegularSemigroup (Weighted model) where
--     inverse (Weighted a) = Weighted (inverse a)
--     
-- instance (ModelParams modelparams model) => ModelParams (Weighted modelparams) (Weighted model) where
--     getparams (Weighted model) = Weighted $ getparams model
--     
-- instance (DefaultParams modelparams model) => DefaultParams (Weighted modelparams) (Weighted model) where
--     defparams = Weighted $ defparams
-- 
-- instance 
--     ( Module ring model
--     , HomTrainer modelparams dp model
--     ) => 
--     HomTrainer (Weighted modelparams) (ring,dp) (Weighted model) 
--         where
--         
--     train1dp' (Weighted params) (ring,dp) = Weighted $ ring .* (train1dp' params dp)