{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

module HLearn.Algebra.Trainers
    where
          
import Data.Semigroup
import Data.Functor
import qualified Data.Foldable as F
          
import HLearn.Algebra.Models
import HLearn.Algebra.Structures
import HLearn.Algebra.Functions
          
-------------------------------------------------------------------------------
-- Classes

class SingletonTrainer modelparams datapoint model | modelparams datapoint -> model where
    train :: modelparams -> datapoint -> model

class WeightedSingletonTrainer modelparams datapoint model | modelparams datapoint -> model where
    trainW :: modelparams -> Weighted datapoint -> model

class BatchTrainer modelparams datapoint model | modelparams datapoint -> model where
    trainBatch :: (Functor container, F.Foldable container) => modelparams -> container datapoint -> model

class WeightedBatchTrainer modelparams datapoint model | modelparams datapoint -> model where
    trainBatchW :: (Functor container, F.Foldable container) => modelparams -> container (Weighted datapoint) -> model

class OnlineTrainer datapoint model where
    trainOnline :: model -> datapoint -> model

class WeightedOnlineTrainer datapoint model where
    trainOnlineW :: model -> Weighted datapoint -> model

-------------------------------------------------------------------------------
-- Instances

-- weighted -> normal

instance (WeightedSingletonTrainer modelparams datapoint model) => SingletonTrainer modelparams datapoint model where
    train modelparams dp = trainW modelparams (dp,1)

{-
-- These result in duplicate instance declarations with the ones below
instance (WeightedBatchTrainer modelparams datapoint model) => BatchTrainer modelparams datapoint model where
    trainBatch modelparams dps = trainBatchW modelparams $ fmap (\dp -> (dp,1)) dps

instance (WeightedOnlineTrainer datapoint model) => OnlineTrainer datapoint model where
    trainOnline model dp = trainOnlineW model (dp,1)
-}

-- normal connections

-- instance (SingletonTrainer modelparams datapoint model, Monoid model)
--             => BatchTrainer modelparams datapoint model where
--     trainBatch modelparams dps = F.foldl' mappend mempty $ fmap (trainSingle modelparams) dps

-- instance (BatchTrainer modelparams datapoint model, Semigroup model, Model modelparams model)
--             => OnlineTrainer datapoint model where
--     trainOnline model dp = model <> trainBatch (params model) [dp]
    
{-instance (SingletonTrainer modelparams datapoint model
         , Epimorphism Semigroup datatpoint model (modelparams -> datapoint -> model))
            => OnlineTrainer datapoint model where
    trainOnline = toOnline trainSingle-}
   
-- instance (Trainer modelparams datapoint model, Monoid model) 
--     => BatchTrainer modelparams datapoint model
--         where
--               
--     trainBatch params xs = F.foldl' mappend mempty $ fmap (train params) xs
--     
-- instance (WeightedTrainer modelparams datapoint model, Monoid model) 
--     => WeightedBatchTrainer modelparams datapoint model
--         where
--               
--     trainBatchW params xs = F.foldl' mappend mempty $ fmap (trainW params) xs


-------------------------------------------------------------------------------
-- Compiler tests

class A a where
    fa :: a -> IO ()
    fa a = putStrLn "fa"
    
class B b where
    fb :: b -> IO ()
    fb b = putStrLn "fb"
    
class C c where
    fc :: c -> IO ()
    fc c = putStrLn "fc"

instance (C i, A i) => B i
instance (C i, B i) => A i

data Test = Test

instance C Test
instance A Test