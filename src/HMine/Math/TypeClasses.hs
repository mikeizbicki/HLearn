{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts, BangPatterns #-}

module HMine.Math.TypeClasses
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
import HMine.DataContainers.DS_List
import HMine.MiscUtils

-------------------------------------------------------------------------------
-- helpers

-- | I only ever expect labels of type Bool, Int, and String, but it may be convenient to use other types as well for something.  This class and instance exist so that we have some reasonable assumptions about what properties labels should have for our other classes to work with.
class (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

instance (Hashable label, Binary label, Ord label, Eq label, Show label, Read label) => Label label

-------------------------------------------------------------------------------
-- Training

class (Label label) => 
    BatchTrainer modelparams model datatype label | modelparams -> model, model -> label 
        where
    
    trainBatch :: 
        ( DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Labeled datatype label) -> HMine model
--     trainBatch :: 
--         ( DataSparse label ds (WLDPS label)
--         , DataSparse label ds (LDPS label)
--         , DataSparse label ds DPS
--         , DataSparse label ds label
--         ) =>
--         modelparams -> ds (LDPS label) -> HMine model


class (Label label) =>
    BatchTrainerSS modelparams model datatype label | modelparams -> model, model -> label
        where
          
    trainBatchSS ::
        ( DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Labeled datatype label) -> ds datatype -> HMine model

---------------------------------------

class (Label label) =>
    WeightedBatchTrainer modelparams model datatype label | modelparams -> model, model -> label 
        where
              
    trainBatchW :: 
        ( DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Weighted (Labeled datatype label)) -> HMine model

class (Label label) =>
    WeightedBatchTrainerSS modelparams model datatype label | modelparams -> model, model -> label 
        where
              
    trainBatchWSS :: 
        ( DataSparse label ds (Weighted datatype)
        , DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Weighted (Labeled datatype label)) -> ds (Weighted datatype) -> HMine model


---------------------------------------

class (Label label) =>
    EmptyTrainer modelparams model label | modelparams -> model, model -> label
        where

    emptyModel :: DataDesc label -> modelparams -> model

class (Label label, EmptyTrainer modelparams model label) => 
    OnlineTrainer modelparams model datatype label | modelparams -> model, model -> label 
        where
    
    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HMine model

    train1dp :: DataDesc label -> modelparams -> Labeled datatype label -> HMine model
    train1dp desc modelparams dp = add1dp desc modelparams (emptyModel desc modelparams) dp
    
    trainOnline :: (DataSparse label ds (Labeled datatype label)) => 
        modelparams -> ds (Labeled datatype label) -> HMine model
    trainOnline modelparams ds = F.foldlM{-Trace-} (add1dp desc modelparams) (emptyModel desc modelparams) ds
        where
            desc = getDataDesc ds
 

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
        modelst <- mkST modelparams $ getDataDesc ds
        foldlMTrace (add1dpST) modelst ds
        freeze modelst

-------------------------------------------------------------------------------
-- Classification

class (Label label) => ProbabilityClassifier model label | model -> label where
    probabilityClassify :: model -> DPS -> [(label,Probability)]
    
    straightClassify :: model -> DPS -> label
    straightClassify model dp = fst . argmaxBy compare snd $ probabilityClassify model dp
    
class (Label label) => Classifier model label | model -> label where
    classify :: model -> DPS -> label

