{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Ensemble.ASSEMBLE
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Semigroup
import Debug.Trace

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import DebugFolds
import HLearn.Base
import HLearn.Math.TypeClasses
import HLearn.Math.Functors
import HLearn.DataContainers
import HLearn.Evaluation.Metrics
import HLearn.MiscUtils
import HLearn.Models.Distributions
import HLearn.Models.DTree
import HLearn.Models.KNN
import HLearn.Models.Ensemble
import HLearn.RandUtils

-------------------------------------------------------------------------------
-- ASSEMBLEParams

data ASSEMBLEParams unweightedparams weightedparams = ASSEMBLEParams 
    { rounds :: Int
    , beta :: Double
    , unweightedParams :: unweightedparams
    , weightedParams :: weightedparams
    }
    deriving (Show,Read,Eq)
    
defAdaBoostParams baseparams = TrainerSS2Trainer $ ASSEMBLEParams
    { rounds = 50
    , beta = 1
    , unweightedParams = CategoricalParams
    , weightedParams = Trainer2WeightedTrainer 0.5 baseparams
    }
    
defASSEMBLEParams = ASSEMBLEParams
    { rounds = 25
    , beta = 0.9
    , unweightedParams = KNNParams 1
    , weightedParams = Trainer2WeightedTrainer 0.5 defDStumpParams
    }
    
instance (NFData unweightedparams, NFData weightedparams) => NFData (ASSEMBLEParams unweightedparams weightedparams) where
    rnf (ASSEMBLEParams rounds beta unweightedParams weightedParams) = 
        deepseq rounds $ deepseq beta $ deepseq unweightedParams $ rnf weightedParams

-------------------------------------------------------------------------------
-- Ensemble instances

instance 
    ( Classifier model DPS label
    , Classifier unweightedmodel DPS label
    , BatchTrainer unweightedparams unweightedmodel DPS label
    , WeightedBatchTrainer weightedparams model DPS label
    ) => 
    BatchTrainerSS (ASSEMBLEParams unweightedparams weightedparams) 
                   (Ensemble (ASSEMBLEParams unweightedparams weightedparams) model label) 
                   DPS
                   label 
        where

    trainBatchSS params@(ASSEMBLEParams rounds beta unweightedParams weightedParams) lds uds = do
        -- step 1,2: initialize weights
        let _D1_l = replicate l (beta / fi l)
        let _D1_u = replicate u ((1-beta) / fi u)
        let _D1 = normalizeL $ _D1_l <> _D1_u
        
        -- step 3,4: initialize pseudo-labels for the unsupervised data
        model0 <- trainBatch unweightedParams lds
        let uds' = fmap (\dps -> (classify model0 dps,dps)) uds
        
        -- step 5-16: iterate and return
        let lds' = lds <> uds'
        model1 <- trainBatchW weightedParams $ zipdsL lds' _D1
        go 1 (emptyEnsemble desc params) model1 lds' _D1
        
        where 
            l = getNumObs lds
            u = getNumObs uds
            desc = getDataDesc lds
            
            go !itr !ens !model !lds !_D
                | itr > rounds = return ens
                | otherwise = do
                    -- step 6
                    let y     = map (                 fst) $ getDataL lds
                    let y_hat = map (classify model . snd) $ getDataL lds
                    
                    -- step 7
                    let err = max 0.01 $ sum [ _Di*(indicator $ yi /= yi_hat) | (_Di,yi,yi_hat) <- zip3 _D y y_hat ]
                        
                    -- step 8
                    
                    -- step 9
                    let w_binary = (1/2)*(log $ (1-err)/err) 
                    let w_adj    = (log $ (fromIntegral $ numLabels desc)-1)
                    let w        = w_binary+w_adj

--                     let w = (1/2) * (log $ (1-err)/err)
                        
                    -- step 10
                    let ens' = pushClassifierNorm (w,model) ens
                        
                    -- step 11
                    let lds' = -- lds
                               snd $
                               T.mapAccumL (\i (label,dps) -> (i+1,
                                                if i < l
                                                    then {-trace ("i="++show i++", LT") -}(label,dps)
                                                    else {-trace ("i="++show i++", GTE")-} (classify model dps, dps)
                                            )) 0 lds
                                            
                    -- step 12
                    let cost' x = -exp (-x)
                    let margin yi _Fib _Fid = (indicator $ yi==_Fib) * _Fid

                    let y' = map fst $ getDataL lds'
                    let (_Fb,_Fd) = unzip $ map (weightedClassify ens' . snd) $ getDataL lds'
                        
                    let _D' = normalizeL [cost' (margin yi _Fib _Fid) | (yi, _Fib, _Fid) <- zip3 y' _Fb _Fd]
                        
                    -- step 13,14
                    let wlds = zipdsL lds' _D'
                    model' <- trainBatchW weightedParams wlds
                        
                    -- iterate
                    trace ("itr="++show itr++", l/u="++show l++"/"++show u++", accuracy="++show (measure Accuracy ens' lds)++", w="++show w++", err="++show err)
                        $ go (itr+1) ens' model' lds' _D'
