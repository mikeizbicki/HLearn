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

module HLearn.Algebra.HomTrainer
    (     
    -- * HomTrainer
    HomTrainer (..)
    , WeightedHomTrainer (..)
    , NumDP(..)
    
    )
    where
          
import qualified Control.ConstraintKinds as CK
import Data.Foldable
          
import HLearn.Algebra.Functions
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- NumDP

-- | numdp returns the number of data points that the model has been trained on
class NumDP model ring | model -> ring where
    numdp :: model -> ring

-------------------------------------------------------------------------------
-- HomTrainer

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class (Semigroup model, Monoid model) => HomTrainer model where
    
    type Datapoint model

    -- | The singleton trainer
    {-# INLINE train1dp #-}
    train1dp :: Datapoint model -> model
    train1dp = unbatch train
    
    -- | The batch trainer
    {-# INLINE train #-}
    train ::     
        ( Functor container
        , Foldable container
        ) => container (Datapoint model) -> model
    train = batch train1dp

    -- | The online trainer
    {-# INLINE add1dp #-}
    add1dp :: model -> Datapoint model -> model
    add1dp model = online train1dp model
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    addBatch ::
        ( Functor container
        , Foldable container
        ) =>  model -> container (Datapoint model) -> model
    addBatch model = online train model

    -- | CK methods take advantage of the ContraintKinds extension to allow containers that require constraints.  In particular, they allow the use of Unboxed Vectors, which can improve performance.
    {-# INLINE trainCK #-}
    trainCK ::     
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container (Datapoint model)
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) => container (Datapoint model) -> model
    trainCK = batchCK train1dp

    {-# INLINE addBatchCK #-}
    addBatchCK ::
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container (Datapoint model)
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container (Datapoint model)
        ) =>  model -> container (Datapoint model) -> model
    addBatchCK model = online trainCK model
    

-------------------------------------------------------------------------------
-- WeightedHomTrainer

class (Module ring model, HomTrainer model) => 
    WeightedHomTrainer ring model 
        where
        
    train1dpW :: (ring,Datapoint model) -> model
    train1dpW (r,dp) = r .* train1dp dp
    
    trainW :: (Foldable container, Functor container) => 
        container (ring,Datapoint model) -> model
    trainW = batch train1dpW

    add1dpW :: model -> (ring,Datapoint model) -> model
    add1dpW = online $ unbatch $ offline addBatchW
    
    addBatchW :: (Foldable container, Functor container) => 
        model -> container (ring,Datapoint model) -> model
    addBatchW = online trainW
    
instance (Module ring model, HomTrainer model) => WeightedHomTrainer ring model
    
-------------------------------------------------------------------------------
-- Counter instance for testing

data Counter sampletype = Counter {c::sampletype}
    deriving (Read,Show,Eq,Ord)
    
instance (Num sampletype) => Semigroup (Counter sampletype) where
    c1<>c2 = Counter $ c c1+c c2

instance (Num sampletype) => Monoid (Counter sampletype) where
    mempty = Counter 0
    mappend = (<>)

instance (Num sampletype) => HomTrainer (Counter sampletype) where
    type Datapoint (Counter sampletype) = sampletype
    train1dp dp = Counter 1
