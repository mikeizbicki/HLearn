{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

module ClassificationAlgebra
    where

import Base
import MiscUtils

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.List.Extras
import Data.Semigroup
import Debug.Trace

class (Monoid m) => CommutativeMonoid m where

-------------------------------------------------------------------------------
-- Training

--     trainIO :: DataDesc -> [(Int,DataPointSparse)] -> IO (NBayes)
-- trainIO desc = trainIOItr 0 (emptyNBayes desc)
--     where
--         trainIOItr :: Int -> NBayes -> [(Int,DataPointSparse)] -> IO (NBayes)
--         trainIOItr itr nb [] = return nb
--         trainIOItr itr nb dpL = do 
--             putStrLn $ "trainIOItr"++show itr
-- --             let nb' = add1dp nb $ head dpL
--             let nb' = unsafeAdd1dp nb $ head dpL
--             seq nb' $ 
--                 trainIOItr (itr+1) nb' (tail dpL)


class OnlineTrainer model label | model -> label where
    train :: DataDesc -> [(label,DataPointSparse)] -> model
    train desc dpL = foldl' (add1dp desc) emptyModel dpL

    train1dp :: DataDesc -> (label,DataPointSparse) -> model
    train1dp desc dp = (add1dp desc) emptyModel dp

    emptyModel :: model
    
-- class OnlineTrainer model label | model -> label where
    add1dp :: DataDesc -> model -> (label,DataPointSparse) -> model
    
-- instance (OnlineTrainer model label) => Trainer model label where
--     emptyModel = error "emptyModel: you must define me!"

class MutableTrainer model modelST label | model -> modelST, modelST -> model, model -> label where
    mkST :: DataDesc -> ST s (modelST s)
    thaw :: model -> ST s (modelST s)
    freeze :: modelST s -> ST s model

    add1dpST :: modelST s -> (label,DataPointSparse) -> ST s (modelST s)

trainST :: (MutableTrainer model modelST label) => DataDesc -> [(label,DataPointSparse)] -> model
trainST desc dpL = runST $ do
    nbst <- mkST desc
    trace "folding trainST" $ return ()
    foldMTrace add1dpST nbst dpL
    trace "freezing trainST" $ return ()
    freeze nbst

-------------------------------------------------------------------------------
-- Classification

class ProbabilityClassifier model label | model -> label where
    probabilityClassify :: model -> DataPointSparse -> [(Int,label)]
    
class Classifier model label | model -> label where
    classify :: model -> DataPointSparse -> label
    
instance (ProbabilityClassifier model label, Ord label) => Classifier model label where
    classify model dp = snd $ argmaxBy compare snd $ probabilityClassify model dp

