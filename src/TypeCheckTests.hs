{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

module Main
    where

class BatchTrainer modelparams model label | modelparams -> model, model -> label where
    trainBatch :: modelparams -> [label] -> model

class WeightedBatchTrainer modelparams model label | modelparams -> model, model -> label 

data Trainer2WeightedTrainer modelparams = Trainer2WeightedTrainer    
instance (BatchTrainer modelparams model label) => WeightedBatchTrainer (Trainer2WeightedTrainer modelparams) model label

class OnlineTrainer modelparams model label | modelparams -> model, model -> label  

data OnlineTrainer2BatchTrainer modelparams = OnlineTrainer2BatchTrainer
instance ( OnlineTrainer modelparams model label) => BatchTrainer (OnlineTrainer2BatchTrainer modelparams) model label 

-------

data Rock rock = Rock rock

let x=OnlineTrainer2BatchTrainer $ Rock (Trainer2WeightedTrainer 0.5 defNBayesParams)