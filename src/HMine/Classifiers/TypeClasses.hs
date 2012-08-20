{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts #-}

module HMine.Classifiers.TypeClasses
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
-- import Control.Monad.ST.Trans
import Control.Monad.Random
import Data.Binary
import Data.Hashable
import Data.List
import Data.List.Extras
import Data.Semigroup
import Debug.Trace

import qualified Data.Foldable as F

import HMine.Base
import HMine.DataContainers
import HMine.MiscUtils

-------------------------------------------------------------------------------
-- helpers

-- | I only ever expect labels of type Bool, Int, and String, but it may be convenient to use other types as well for something.  This class and instance exist so that we have some reasonable assumptions about what properties labels should have for our other classes to work with.
class (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

instance (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

-------------------------------------------------------------------------------
-- Training

class (Label label) => 
    BatchTrainer modelparams model label | modelparams -> model, model -> label 
        where
    
    trainBatch :: 
        ( DataSparse label ds (WDPS label)
        , DataSparse label ds (LDPS label)
        , DataSparse label ds DPS
        , DataSparse label ds label
        ) =>
        modelparams -> ds (LDPS label) -> HMine model

-- data SemigroupTrainer modelparams = SemigroupTrainer
--     { numSemigroups :: Int
--     , sgModelParams :: modelparams
--     }
--     
-- instance (Label label, Semigroup model) => BatchTrainer (SemigroupTrainer modelparams) model label where
--     trainBatch = undefined

class (Label label) =>
    BatchTrainerSS modelparams model label | modelparams -> model, model -> label
        where
          
    trainBatchSS ::
        ( DataSparse label ds (WDPS label)
        , DataSparse label ds (LDPS label)
        , DataSparse label ds DPS
        , DataSparse label ds label
        ) =>
        modelparams -> ds (LDPS label) -> ds DPS -> HMine model

data Trainer2TrainerSS modelparams = Trainer2TrainerSS { ttssModelParams :: modelparams }

instance (BatchTrainer modelparams model label) => BatchTrainerSS (Trainer2TrainerSS modelparams) model label
    where
          
    trainBatchSS (Trainer2TrainerSS modelparams) lds uds = trainBatch modelparams lds

---------------------------------------

class (Label label) =>
    WeightedBatchTrainer modelparams model label | modelparams -> model, model -> label 
        where
              
    trainBatchW :: 
        ( DataSparse label ds (WDPS label)
        , DataSparse label ds (LDPS label)
        , DataSparse label ds DPS
        , DataSparse label ds label
        ) =>
        modelparams -> ds (WDPS label) -> HMine model

data Trainer2WeightedTrainer modelparams = Trainer2WeightedTrainer
    { sampleRate :: Double
    , sampleModelparams :: modelparams
    }
    deriving (Read,Show)
    
instance (NFData modelparams) => NFData (Trainer2WeightedTrainer modelparams) where
    rnf (Trainer2WeightedTrainer params rate) = deepseq params $ rnf rate
    
instance (BatchTrainer modelparams model label) =>
    WeightedBatchTrainer (Trainer2WeightedTrainer modelparams) model label
        where

    trainBatchW params wds = do
        wds' <- sample (floor $ (sampleRate params)*(fromIntegral $ getNumObs wds)) wds
        trainBatch (sampleModelparams params) wds'
--         trainBatch (sampleModelparams params) $ fmap fst wds

--     trainBatchW params wds = do
{-        let numSamples = floor $ (sampleRate params)*(fromIntegral $ getNumObs wds)
        weightL <- liftM sort $ replicateM numSamples $ getRandomR (0,1)
        let wds' = F.foldl' sample () wds-}
--         wds' <- sample 
--         trainBatch (sampleModel params) wds'
        
---------------------------------------

class (Label label) => OnlineTrainer modelparams model label | modelparams -> model, model -> label where
    
    emptyModel :: DataDesc label -> modelparams -> model
    add1dp :: DataDesc label -> modelparams -> model -> LDPS label -> HMine model

    train1dp :: DataDesc label -> modelparams -> LDPS label -> HMine model
    train1dp desc modelparams dp = add1dp desc modelparams (emptyModel desc modelparams) dp
    
    trainOnline :: (DataSparse label ds (LDPS label)) => 
        modelparams -> ds (LDPS label) -> HMine model
    trainOnline modelparams ds = {-F.-}foldlMTrace (add1dp desc modelparams) (emptyModel desc modelparams) ds
        where
            desc = getDataDesc ds
 
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


---------------------------------------

class (Label label) => 
    MutableTrainer modelparams model modelST label 
        | modelparams -> model
        , model -> modelparams
        , model -> modelST
        , modelST -> model
        , model -> label 
            where
    
    mkST :: modelparams -> DataDesc label -> ST s (modelST s)
    thaw :: model -> ST s (modelST s)
    freeze :: modelST s -> ST s model

    add1dpST :: modelST s -> LDPS label -> ST s (modelST s)

    trainST :: (DataSparse label ds (LDPS label)) => 
        modelparams -> ds (LDPS label) -> model
    trainST modelparams ds = {-return $ -}runST $ do
--         trace ("mkST: "++(show $ getDataDesc ds)) $ return ()
        modelst <- mkST modelparams $ getDataDesc ds
--         trace "folding trainST" $ return ()
        foldlMTrace (add1dpST) modelst ds
--         trace "freezing trainST" $ return ()
        freeze modelst

-------------------------------------------------------------------------------
-- Classification

class (Label label) => ProbabilityClassifier model label | model -> label where
    probabilityClassify :: model -> DPS -> [(label,Probability)]
    
    straightClassify :: model -> DPS -> label
    straightClassify model dp = fst . argmaxBy compare snd $ probabilityClassify model dp
    
class (Label label) => Classifier model label | model -> label where
    classify :: model -> DPS -> label

data ProbabilityClassifier2Classifier model = ProbabilityClassifier2Classifier { straightModel :: model }

instance (ProbabilityClassifier model label) => Classifier (ProbabilityClassifier2Classifier model) label where
    classify = straightClassify . straightModel

-- instance (ProbabilityClassifier model label) => Classifier model label where
--     classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

