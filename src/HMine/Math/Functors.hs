{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module HMine.Math.Functors
    where
          
import Control.DeepSeq
import Control.Monad
import Data.List
          
import HMine.Base
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- Semigroups

data SemigroupTrainer modelparams = SemigroupTrainer
    { numSemigroups :: Int
    , sgModelParams :: modelparams
    }
    deriving (Read,Show,Eq)
    
-- instance (Semigroup model, BatchTrainer modelparams model label) => BatchTrainer (SemigroupTrainer modelparams) model label where
--     trainBatch (SemigroupTrainer num modelparams) lds = foldl1' (liftM2 (<>)) $ map (trainBatch modelparams) (splitds num lds)

instance (NFData model, Semigroup model, BatchTrainerSS modelparams model label) => BatchTrainerSS (SemigroupTrainer modelparams) model label where
{-    trainBatchSS (SemigroupTrainer num modelparams) lds uds = {-liftM2 deepseq (sequence ret) $-} error $ "done. length ret="++show (length ret)++", length ldsL="++show ({-length-} ldsL)++", length udsL="++show (length udsL)
        where
            ret = map (\(i,lds',uds') -> trace ("sg.trainBatchSS.i="++show i) $ trainBatchSS modelparams lds' uds') $ zip3 [1..] ldsL udsL
            ldsL = splitds num lds
            udsL = splitds num uds-}
              
        
    trainBatchSS (SemigroupTrainer num modelparams) lds uds = 
        foldl1' (liftM2 (<>)) $ map (\(lds',uds') -> trainBatchSS modelparams lds' uds') $ zip ldsL udsL
        where                
--             ret = liftM (foldl1' (liftM2 (<>))) $ mapM (\(i,lds',uds') -> trace ("sg.trainBatchSS.i="++show i) $ trainBatchSS modelparams lds' uds') $ zip3 [1..] ldsL udsL
            ldsL = splitds num lds
            udsL = splitds num uds

-------------------------------------------------------------------------------
-- Supervised / Semi-Supervised conversion

data Trainer2TrainerSS modelparams = Trainer2TrainerSS { ttssModelParams :: modelparams }
    deriving (Read,Show,Eq)

instance (BatchTrainer modelparams model label) => BatchTrainerSS (Trainer2TrainerSS modelparams) model label
    where
          
    trainBatchSS (Trainer2TrainerSS modelparams) lds uds = trainBatch modelparams lds

data TrainerSS2Trainer modelpatams = TrainerSS2Trainer { tsstModelPatams :: modelpatams }
    deriving (Read,Show,Eq)

instance (BatchTrainerSS modelparams model label) => BatchTrainer (TrainerSS2Trainer modelparams) model label
    where
          
    trainBatch (TrainerSS2Trainer modelparams) lds = trainBatchSS modelparams lds $ emptyds $ getDataDesc lds

-------------------------------------------------------------------------------
-- Weighted / Unweighted conversion

data Trainer2WeightedTrainer modelparams = Trainer2WeightedTrainer
    { sampleRate :: Double
    , sampleModelparams :: modelparams
    }
    deriving (Read,Show,Eq)
    
instance (NFData modelparams) => NFData (Trainer2WeightedTrainer modelparams) where
    rnf (Trainer2WeightedTrainer params rate) = deepseq params $ rnf rate
    
instance (BatchTrainer modelparams model label) =>
    WeightedBatchTrainer (Trainer2WeightedTrainer modelparams) model label
        where

    trainBatchW params wds = do
        wds' <- sample (floor $ (sampleRate params)*(fromIntegral $ getNumObs wds)) wds
        trainBatch (sampleModelparams params) wds'

instance (BatchTrainerSS modelparams model label) =>
    WeightedBatchTrainerSS (Trainer2WeightedTrainer modelparams) model label
        where

    trainBatchWSS params wlds wuds = do
        wlds' <- sample numwlds wlds
        wuds' <- sample numwuds wuds
        trainBatchSS (sampleModelparams params) wlds' wuds'
        
        where 
            l = fromIntegral $ getNumObs wlds
            u = fromIntegral $ getNumObs wlds
            numwlds = floor $ (sampleRate params)*(l+u)*(l/u)
            numwuds = floor $ (sampleRate params)*(l+u)*(u/l)

-------------------------------------------------------------------------------
-- Online / Batch conversion

data OnlineTrainer2BatchTrainer modelparams = OnlineTrainer2BatchTrainer
    { ot2btModelParams :: modelparams
    }

instance (NFData modelparams) => NFData (OnlineTrainer2BatchTrainer modelparams) where
    rnf (OnlineTrainer2BatchTrainer params) = rnf params
    
instance 
    ( OnlineTrainer modelparams model label
    ) => 
        BatchTrainer (OnlineTrainer2BatchTrainer modelparams) model label 
            where
    
    trainBatch = trainOnline . ot2btModelParams

-------------------------------------------------------------------------------
-- Classifier Conversion

data ProbabilityClassifier2Classifier model = ProbabilityClassifier2Classifier { straightModel :: model }

instance (ProbabilityClassifier model label) => Classifier (ProbabilityClassifier2Classifier model) label where
    classify = straightClassify . straightModel

-- instance (ProbabilityClassifier model label) => Classifier model label where
--     classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp