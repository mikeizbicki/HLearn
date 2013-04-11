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

-- | Every model in the HLearn library is an instance of the 'HomTrainer' type class.  This ensures that the batch trainer is a monoid homomorphism.  This is a restrictive condition that not all learning models satisfy; however, it is useful for two reasons.  First, this property lets us easily derive three important functions for machine learning algorithms: online trainers, parallel trainers, and fast cross-validation algorithms.  Second, many popular algorithms (or variants on them) satisfy the condition and are implemented in the library.
--
-- For a full theoretical description of the 'HomTrainer' class, see the paper: <INSERT HERE>
--
-- Unfortunately, the class hierarchy here is slightly more complicated.  In the paper, we assume that all parameters for a model can be included in the model's type.  Currently, however, this is not possible in Haskell, so every model must also have a data type that describes it's parameters.  This is the purpose of the 'ModelParams' class.  Most models have either no parameters, or reasonable defaults, and so their parameters are instances of the 'DefaultParams' class.

module HLearn.Algebra.Models
    ( 
    -- * Parameters
    ModelParams (..)
    , DefaultParams (..)
    , NoParams (..)
    
    -- * HomTrainer
    , HomTrainer (..)
    , DefaultHomTrainer (..)
    
    )
    where
          
import qualified Control.ConstraintKinds as CK
import Data.Foldable
          
import HLearn.Algebra.Functions
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- ModelParams

-- | Every model has at least one data type that that fully describes its parameters.  Many models do not actually *need* any parameters, in which case they can use the type 'NoParams'.
class ModelParams model where
    type Params model 
    getparams :: model -> Params model -- ^ Given a model, returns the parameters used to generate the model

-- | This data type can be used to specify that the model requires no parameters.
data NoParams = NoParams
    deriving (Read,Show,Eq,Ord)

-- | Many models have reasonable default parameters.  By making our parameters instances of 'DefaultParams', our models will become instances of 'DefaultHomTrainer' automatically.
class DefaultParams params where
    defparams :: params -- ^ the default parameters
    
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