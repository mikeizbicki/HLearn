{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

module Main
    where

{-import Control.Monad.Random
import Criterion.Main
import Data.List

import HMine.Testing

eval f xs = (foldl' (+) 0 l1, foldl' (+) 0 l2)
    where (l1,l2) = evalRand (f 0.5 xs) (mkStdGen 10)
    
evalSafe = eval randSplit
evalUnsafe = eval randSplitUnsafe
    
main = defaultMain 
    [ {-bgroup "randSplit" 
        [ bench "100"     $ nf evalSafe [0..100::Int]
        , bench "1000"    $ nf evalSafe [0..1000::Int]
        , bench "10000"   $ nf evalSafe [0..10000::Int]
        , bench "100000"  $ nf evalSafe [0..100000::Int]
        , bench "1000000" $ nf evalSafe [0..1000000::Int]
        ]
    , -}bgroup "randSplitUnsafe" 
        [ bench "100"     $ nf evalUnsafe [0..100::Int]
        , bench "1000"    $ nf evalUnsafe [0..1000::Int]
        , bench "10000"   $ nf evalUnsafe [0..10000::Int]
        , bench "100000"  $ nf evalUnsafe [0..100000::Int]
        , bench "1000000" $ nf evalUnsafe [0..1000000::Int]
        ]
    ]-}
    
    
    
-------------------------------------------------------------------------------


class BatchTrainer modelparams model label | modelparams -> model, model -> label 
class WeightedBatchTrainer modelparams model label | modelparams -> model, model -> label 

data Trainer2WeightedTrainer modelparams = Trainer2WeightedTrainer    
instance (BatchTrainer modelparams model label) => WeightedBatchTrainer (Trainer2WeightedTrainer modelparams) model label

class OnlineTrainer modelparams model label | modelparams -> model, model -> label  

data OnlineTrainer2BatchTrainer modelparams = OnlineTrainer2BatchTrainer
instance ( OnlineTrainer modelparams model label) => BatchTrainer (OnlineTrainer2BatchTrainer modelparams) model label 
