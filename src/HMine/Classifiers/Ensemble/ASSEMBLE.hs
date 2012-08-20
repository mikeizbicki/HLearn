{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Classifiers.Ensemble.AdaBoost
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import qualified Data.Foldable as F
import Data.List
import Debug.Trace

import DebugFolds
import HMine.Base
import HMine.Classifiers.Ensemble
import HMine.Classifiers.TypeClasses
import HMine.DataContainers
import HMine.RandUtils
import HMine.Testing

-------------------------------------------------------------------------------
-- ASSEMBLEParams

data ASSEMBLEParams modelparams = ASSEMBLEParams 
    { rounds :: Int
    , baseModel :: modelparams
    }
    deriving (Show,Read,Eq)
    
instance (NFData modelparams) => NFData (ASSEMBLEParams modelparams) where
    rnf (ASSEMBLEParams rounds model) = deepseq rounds $ rnf model

-------------------------------------------------------------------------------
-- Ensemble instances

instance (Classifier model label, WeightedBatchTrainer modelparams model label) => 
    BatchTrainer (AdaBoostParams modelparams) (Ensemble (AdaBoostParams modelparams) model label) label 
        where
              
    trainBatch adaparams ds = do
        let m = getNumObs ds
        let _D0 = replicate m (1/fromIntegral m)
        (_,_,ens) <- go (1,_D0,emptyEnsemble (getDataDesc ds) adaparams)
        return ens

        where
            go (itr,_D,ens)
                | itr>(adaRounds adaparams) = return (itr,_D,ens)
                | otherwise = do
                    model <- trainBatchW (adaBaseModel adaparams) $ zipds ds _D
                    let err = sum [(_Di)*(indicator $ label/=classify model dps) | (_Di,(label,dps)) <- zip _D (getDataL ds)]
                    let w = (1/2)*(log $ (1-err)/err) {-+ (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1)-}

                    let ens'=pushClassifier (w,model) ens

--                     let _D_numer = [cost' adaparams $ bool2num $ label==classify ens' dp | (label,dp) <- getDataL ds]
--                     let _D_numer = fmap (\(label,dp) -> (cost' adaparams) $ bool2num $ label==classify ens' dp) $ getDataL ds
                    let _D_numer = fmap (\(label,dp) -> (cost' adaparams) $ (w*) $ indicator $ label/=classify ens dp) $ getDataL ds
                    let _D_denom = sum _D_numer
                    let _D' = fmap (/_D_denom) _D_numer
                        
                    trace ("itr="++show itr++", accuracy="++show (accuracy ens ds)++", err="++show err++", extra="++show (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1)++", sum_D="++show (sum _D)++", _D="++show (take 10 _D))
                        $ go (itr+1,_D',ens')

