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
import Control.Parallel.Strategies
import Data.List
import Data.List.Extras
import Debug.Trace
          
import HMine.Base
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.MiscUtils
import HMine.Models.Ensemble

-------------------------------------------------------------------------------
-- DataDivider

data DataDivider modelparams = DataDivider Double modelparams

instance (NFData modelparams) => NFData (DataDivider modelparams) where
    rnf (DataDivider divisor modelparams) = rnf modelparams
    
instance (BatchTrainer modelparams model datatype label) => 
    BatchTrainer (DataDivider modelparams) model datatype label
        where
        
    trainBatch (DataDivider divisor modelparams) ds = trainBatch modelparams $ takeFirst len ds
        where len = round $ (fi $ getNumObs ds)/divisor
    

-------------------------------------------------------------------------------
-- DebugFunctors

newtype DebugFunctor modelparams = DebugFunctor modelparams
    deriving (Show,Read,Eq)

instance (NFData modelparams) => NFData (DebugFunctor modelparams) where
    rnf (DebugFunctor modelparams) = rnf modelparams
    
instance (BatchTrainer modelparams model datatype label) => 
    BatchTrainer (DebugFunctor modelparams) model datatype label
        where
        
    trainBatch (DebugFunctor modelparams) ds = 
        trace "DebugFunctor.trainBatch:" $
        trace (" >> datadesc="++(show $ getDataDesc ds)) $
        trace (" >> numObs="++(show $ getNumObs ds)) $
        trainBatch modelparams ds

-------------------------------------------------------------------------------
-- Semigroups

data SemigroupTrainer modelparams = SemigroupTrainer
    { numSemigroups :: Int
    , ldsRedundancy :: Int
    , udsRedundancy :: Int
    , sgModelParams :: modelparams
    }
    deriving (Read,Show,Eq)

defSemigroupTrainer numsg modelparams = SemigroupTrainer
    { numSemigroups = numsg
    , ldsRedundancy = 1
    , udsRedundancy = 1
    , sgModelParams = modelparams
    }

defSemigroupTrainerSS numsg modelparams = SemigroupTrainer
    { numSemigroups = numsg
    , ldsRedundancy = numsg
    , udsRedundancy = 1
    , sgModelParams = modelparams
    }

instance (NFData model, Semigroup model, BatchTrainerSS modelparams model datatype label) => 
    BatchTrainerSS (SemigroupTrainer modelparams) model datatype label 
        where
              
    trainBatchSS (SemigroupTrainer numsg ldsredun udsredun modelparams) lds uds = 
        reduce $ map (\(lds',uds') -> trainBatchSS modelparams lds' uds') $ zip ldsL udsL
        where                
            ldsL = splitdsRedundantSimple numsg ldsredun lds
            udsL = splitdsRedundantSimple numsg udsredun uds

instance (NFData model, Semigroup model, BatchTrainer modelparams model datatype label) => 
    BatchTrainer (SemigroupTrainer modelparams) model datatype label 
        where
              
    trainBatch (SemigroupTrainer numsg ldsredun udsredun modelparams) lds = 
        reduce $ map (trainBatch modelparams) ldsL
        where                
            ldsL = splitdsRedundantSimple numsg ldsredun lds

reduce :: (Semigroup sg) => [sg] -> sg
reduce [x] = x
reduce xs  = reduce $ reducestep xs

reducestep :: (Semigroup sg) => [sg] -> [sg]
reducestep []       = []
reducestep (x:[])   = [x]
reducestep (x:y:xs) = runEval $ do
    z  <- rpar $ (x<>y)
    zs <- rseq $ reducestep xs
    return $ z:zs

-------------------------------------------------------------------------------
-- Voting Semigroups

data VotingSemigroupParams modelparams = VotingSemigroupParams modelparams
    deriving (Read,Show,Eq)

instance (NFData modelparams) => NFData (VotingSemigroupParams modelparams)
    where
        rnf (VotingSemigroupParams params) = rnf params
        
instance (BatchTrainer modelparams model datatype label) => 
    BatchTrainer (VotingSemigroupParams modelparams) (VotingSemigroupModel modelparams model label) datatype label
        where

    trainBatch vsgparams@(VotingSemigroupParams params) dps = do
        model <- trainBatch params dps
        return $ VotingSemigroupModel $ EnsembleAppender $ Ensemble 
            { ensembleL = [(1,model)]
            , ensembleDataDesc = getDataDesc dps
            , ensembleParams = {-vsgparams-} params
            }

---------------------------------------

data VotingSemigroupModel modelparams model label = VotingSemigroupModel (EnsembleAppender modelparams model label)
    deriving (Read,Show,Eq)

instance (NFData (EnsembleAppender modelparams model label)) => NFData (VotingSemigroupModel modelparams model label)
    where
        rnf (VotingSemigroupModel ensapp) = rnf ensapp

instance (Label label, Eq modelparams) => Semigroup (VotingSemigroupModel modelparams model label) where
    (<>) (VotingSemigroupModel ensapp1) (VotingSemigroupModel ensapp2) = 
        VotingSemigroupModel (ensapp1 <> ensapp2)

instance (Label label, Classifier model datatype label) => 
    Classifier (VotingSemigroupModel modelparams model label) datatype label 
        where
    classify (VotingSemigroupModel (EnsembleAppender model)) dp = --undefined 
        fst $ argmax snd  $ histogram $ map (flip classify dp . snd) $ ensembleL model

-------------------------------------------------------------------------------
-- Supervised / Semi-Supervised conversion

data Trainer2TrainerSS modelparams = Trainer2TrainerSS { ttssModelParams :: modelparams }
    deriving (Read,Show,Eq)

instance (BatchTrainer modelparams model datatype label) => 
    BatchTrainerSS (Trainer2TrainerSS modelparams) model datatype label
        where
          
    trainBatchSS (Trainer2TrainerSS modelparams) lds uds = trainBatch modelparams lds

data TrainerSS2Trainer modelparams = TrainerSS2Trainer { tsstModelPatams :: modelparams }
    deriving (Read,Show,Eq)

instance (NFData modelparams) => NFData (TrainerSS2Trainer modelparams) where
    rnf (TrainerSS2Trainer modelparams) = rnf modelparams

instance (BatchTrainerSS modelparams model datatype label) => 
    BatchTrainer (TrainerSS2Trainer modelparams) model datatype label
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
    
instance (BatchTrainer modelparams model datatype label) =>
    WeightedBatchTrainer (Trainer2WeightedTrainer modelparams) model datatype label
        where

    trainBatchW params wds = do
        wds' <- sample (floor $ (sampleRate params)*(fromIntegral $ getNumObs wds)) wds
        trainBatch (sampleModelparams params) wds'

instance (BatchTrainerSS modelparams model datatype label) =>
    WeightedBatchTrainerSS (Trainer2WeightedTrainer modelparams) model datatype label
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
    ( OnlineTrainer modelparams model datatype label
    ) => 
        BatchTrainer (OnlineTrainer2BatchTrainer modelparams) model datatype label 
            where
    
    trainBatch = trainOnline . ot2btModelParams

-------------------------------------------------------------------------------
-- Classifier Conversion

-- data ProbabilityClassifier2Classifier model = ProbabilityClassifier2Classifier { straightModel :: model }
-- 
-- instance (ProbabilityClassifier model datatype label) => 
--     Classifier (ProbabilityClassifier2Classifier model) datatype label 
--         where
--     classify = straightClassify . straightModel

-- instance (ProbabilityClassifier model label) => Classifier model label where
--     classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp