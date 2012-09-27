{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances, FlexibleContexts, BangPatterns #-}

module HLearn.Math.TypeClasses
    where

import Control.Monad.ST
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Foldable as F

import HLearn.Base
import HLearn.DataContainers
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical

-------------------------------------------------------------------------------
-- Training

class 
    (Label label) => 
--     (Model model label) => 
    BatchTrainer modelparams model datatype label | modelparams -> model, model -> label 
        where
    
    trainBatch :: 
        ( DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Labeled datatype label) -> HLearn model

class (Label label) =>
    BatchTrainerSS modelparams model datatype label | modelparams -> model, model -> label
        where
          
    trainBatchSS ::
        ( DataSparse label ds (Weighted (Labeled datatype label))
        , DataSparse label ds (Labeled datatype label)
        , DataSparse label ds datatype
        , DataSparse label ds label
        ) =>
        modelparams -> ds (Labeled datatype label) -> ds datatype -> HLearn model

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
        modelparams -> ds (Weighted (Labeled datatype label)) -> HLearn model

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
        modelparams -> ds (Weighted (Labeled datatype label)) -> ds (Weighted datatype) -> HLearn model


---------------------------------------

class (Label label) =>
    EmptyTrainer modelparams model label | modelparams -> model, model -> label
        where

    emptyModel :: DataDesc label -> modelparams -> model

class (Label label, EmptyTrainer modelparams model label) => 
    OnlineTrainer modelparams model datatype label | modelparams -> model, model -> label 
        where
    
    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HLearn model

    train1dp :: DataDesc label -> modelparams -> Labeled datatype label -> HLearn model
    train1dp desc modelparams dp = add1dp desc modelparams (emptyModel desc modelparams) dp
    
    trainOnline :: (DataSparse label ds (Labeled datatype label)) => 
        modelparams -> ds (Labeled datatype label) -> HLearn model
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
--     trainST modelparams ds = {-return $ -}runST $ do
--         modelst <- mkST modelparams $ getDataDesc ds
--         foldlMTrace (add1dpST) modelst ds
--         freeze modelst

-------------------------------------------------------------------------------
-- Classification

class (Label label) => ProbabilityClassifier model datatype label | model -> label where
--     probabilityClassify :: model -> datatype -> [(label,Probability)]
--     probabilityClassify :: (Distribution dist label) => model -> datatype -> dist
    probabilityClassify :: model -> datatype -> Categorical label
    
--     straightClassify :: model -> datatype -> label
--     straightClassify = mean . probabilityClassify
--     straightClassify model dp = classificationLabel $ probabilityClassify model dp
--     straightClassify model dp = fst . argmaxBy compare snd $ probabilityClassify model dp
    
class (Label label) => Classifier model datatype label | model -> label where
    classify :: model -> datatype -> label

-- instance (ProbabilityClassifier model datatype label) => Classifier model datatype label where
--     classify model dp = mean $ probabilityClassify model dp

classificationEntropy :: [(label,Probability)] -> Double
classificationEntropy = sum . map (\(l,p) -> (fromLogFloat p)*(logFromLogFloat p)) 