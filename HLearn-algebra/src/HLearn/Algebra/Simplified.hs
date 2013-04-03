{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HLearn.Algebra.Simplified
    where
          
import Data.Foldable
import Data.Monoid
import Data.Semigroup
import HLearn.Algebra.Functions
import HLearn.Algebra.Structures.Modules

-- | A minimal complete definition of the class is the singleton trainer 'train1dp\''
class (Semigroup model, Monoid model) => 
    HomTrainer datapoint model | model -> datapoint
        where

    -- | The singleton trainer
    train1dp :: datapoint -> model
    train1dp = offline add1dp
    
    -- | The batch trainer
    train :: ( Functor container, Foldable container) => 
        container datapoint -> model
    train = batch train1dp

    -- | The online trainer
    add1dp :: model -> datapoint -> model
    add1dp = online $ unbatch $ offline addBatch
    
    -- | The batch online trainer; will be more efficient than simply calling 'add1dp' for each element being added
    addBatch :: (Functor container, Foldable container) =>  
        model -> container datapoint -> model
    addBatch = online train

class (Module ring model, HomTrainer datapoint model) => 
    WeightedHomTrainer ring datapoint model 
        where
              
    train1dpW :: (ring,datapoint) -> model
    train1dpW (r,dp) = r .* train1dp dp
    
    trainW :: (Foldable container, Functor container) => 
        container (ring,datapoint) -> model
    trainW = batch train1dpW

    add1dpW :: model -> (ring,datapoint) -> model
    add1dpW = online $ unbatch $ offline addBatchW
    
    addBatchW :: (Foldable container, Functor container) => 
        model -> container (ring,datapoint) -> model
    addBatchW = online trainW