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
import HMine.DataContainers
import HMine.Evaluation.Metrics
import HMine.Math.TypeClasses
import HMine.MiscUtils
import HMine.RandUtils

-------------------------------------------------------------------------------
-- AdaBoostParams

data AdaBoostParams modelparams = AdaBoostParams 
    { adaRounds :: Int
    , adaBaseModel :: modelparams
    }
    deriving (Show,Read,Eq)
    
instance (NFData modelparams) => NFData (AdaBoostParams modelparams) where
    rnf (AdaBoostParams rounds model) = deepseq rounds $ rnf model

-- class BoostAlg modelparams where
--     cost  :: modelparams -> Double -> Double
--     cost' :: modelparams -> Double -> Double
--     
-- instance BoostAlg (AdaBoostParams modelparams) where
--     cost  _ x =  exp (-x)
--     cost' _ x = -exp (-x)

-------------------------------------------------------------------------------
-- Ensemble instances

instance (Show model, Show modelparams, Classifier model DPS label, {-Weighted-}BatchTrainer modelparams model DPS label) => 
    BatchTrainer (AdaBoostParams modelparams) (Ensemble (AdaBoostParams modelparams) model label) DPS label 
        where
              
    trainBatch adaparams ds = trace ("getNumObs ds="++show (getNumObs ds)) $ do
        let m = getNumObs ds
        let _D0 = replicate m (1/fi m)
        go 1 (emptyEnsemble (getDataDesc ds) adaparams) _D0
        
        where
            go !itr !ens !_D 
                | itr>(adaRounds adaparams) = return ens
                | otherwise = trace "-----" $ do
                    ds' <- sample (100) $ zipdsL ds _D
--                     let ds'=ds
                    model <- --trace ("ds'="++(show $ takeFirst 10 ds')) $
                        trainBatch (adaBaseModel adaparams) ds'
--                     model <- trainBatchW (adaBaseModel adaparams) $ zipdsL ds _D
                    let err = max 0.01 $ sum [(_Di)*(indicator $ label/=classify model dps) | (_Di,(label,dps)) <- zip _D (getDataL ds)]
                    let w_binary = (1/2)*(log $ (1-err)/err) 
                    let w_adj    = (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1)
                    let w        = w_binary+w_adj

                    let ens'=pushClassifier{-Norm-} (w,model) ens

                    let margin (yi,(_Fib,_Fid)) = (bool2num $ yi==_Fib)*_Fid
                    let cost' x = -exp (-x)
--                     let _D' = normalizeL [ cost' $ margin (label,weightedClassify ens' dp) | (label,dp) <- getDataL ds]
                    let _D' = normalizeL [ _Di * (exp $ (w)*(indicator $ label/=classify model dp)) 
                                         | (_Di, (label,dp)) <- zip _D $ getDataL ds
                                         ]
                        
                    trace ("itr="++show itr++", accuracy="++show (accuracy ens' ds)++", err="++show err++", extra="++show (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1))
--                         $ trace ("  -> sum_D ="++show (sum _D)++", _D="++show (take 10 _D))
--                         $ trace ("  -> sum_D'="++show (sum _D')++", (y,_D')="++show (take 10 $ zip (map fst $ getDataL ds) _D'))
--                         $ trace "" 
--                         $ trace ("ens'="++show (map fst $ ensembleL $ ens'))
--                         $ trace ("model="++show model)
                        $ trace ""
                        $ trace ("CM-mod="++show (genConfusionMatrix model ds))
                        $ trace ("CM-ens="++show (genConfusionMatrix ens' ds))
                        $ go (itr+1) ens' _D'

--     trainBatch adaparams ds = do
--         let m = getNumObs ds
--         let _D0 = replicate m (1/fromIntegral m)
--         (_,_,ens) <- go' adaparams ds (1,_D0,emptyEnsemble (getDataDesc ds) adaparams)
--         return ens
-- {-        ensembleL' <- unfoldrM (go' adaparams ds) (1,_D0,emptyEnsemble (getDataDesc ds) adaparams) -- (emptyEnsemble adaparams)
--         return $ Ensemble 
--             { ensembleL = ensembleL' 
--             , ensembleDataDesc = getDataDesc ds
--             , ensembleParams = adaparams
--             }-}
--             
--         where 
-- 
-- go' adaparams ds = go
--     where
--         go (itr,_D,ens)
--             | itr>(adaRounds adaparams) = return Nothing
--             | otherwise = trace ("itr="++show itr++", accuracy="++show (accuracy ens ds)++", extra="++show (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1)++", sum_D="++show (sum _D)++", _D="++show (take 10 _D))
--                         $ do
--                 model <- trainBatchW (adaBaseModel adaparams) $ zipds ds _D
--                 let err = sum [(_Di)*(indicator $ label/=classify model dps) | (_Di,(label,dps)) <- zip _D (getDataL ds)]
--                 let w = (1/2)*(log $ (1-err)/err) + (log $ (fromIntegral $ numLabels $ getDataDesc ds)-1)
-- 
--                 let nextClassifier = (w,model)
--                 let ens'=pushClassifier nextClassifier ens
-- 
-- --                 let _D_numer = fmap (\(label,dp) -> (cost' adaparams) $ bool2num $ label==classify ens' dp) $ getDataL ds
--                 let _D_numer = fmap (\(label,dp) -> (cost' adaparams) $ (w*) $ indicator $ label/=classify ens dp) $ getDataL ds
--                 let _D_denom = F.foldl' (+) 0 _D_numer
--                 let _D' = fmap (\x -> x/_D_denom) _D_numer
--                     
--                 return $ Just (nextClassifier,(itr+1,_D',ens'))


instance (EmptyTrainer modelparams model label) =>
    EmptyTrainer (AdaBoostParams modelparams) (Ensemble (AdaBoostParams modelparams) model label) label
        where
    
    emptyModel desc params = Ensemble
        { ensembleL = map (\x -> (1,emptyModel desc x)) $ replicate (adaRounds params) (adaBaseModel params)
        , ensembleDataDesc = desc
        , ensembleParams = params
        }
    

instance (Classifier model DPS label, OnlineTrainer modelparams model DPS label) =>
    OnlineTrainer (AdaBoostParams modelparams) (Ensemble (AdaBoostParams modelparams) model label) DPS label where
    
--     add1dp :: DataDesc -> modelparams -> model -> LDPS label -> HMine model            
    add1dp desc modelparams ens ldp@(y,x) = do
        ensembleL' <- go (1::Double) $ ensembleL ens
        return $ ens { ensembleL=ensembleL' }
        where
--             go :: (RandomGen g) => Double -> [(Double,model)] -> Rand g [(Double,model)]
            go _ [] = return []
            go !lambda !((w,f_model):ensL) = {-trace ("ensL="++(show $ length ensL)++"; lambda="++(show lambda)++"; desc="++(show desc)) $ -}do
                k <- poisson lambda
                let sigma=1
                let lambda_sc = sigma*(exp w)/(1+exp w)
                let lambda_sw = sigma-lambda_sc

                f_model' <- foldM (\model i -> add1dp desc (adaBaseModel modelparams) model ldp) f_model [1..k]
                let correct=y==classify f_model' x

                let lambda_sc' = if correct
                        then lambda_sc+lambda
                        else lambda_sc

                let lambda_sw' = if correct
                        then lambda_sw+lambda
                        else lambda_sw

                let epsilon = lambda_sw'/(lambda_sc'+lambda_sw')

                let lambda' = if correct
                        then lambda*(1/(2*(1-epsilon)))
                        else lambda*(1/(2*epsilon))

                ensL' <- {-trace ("k="++show k++"; epsilon="++show epsilon) $-} go lambda' ensL
--                 ensL' <- trace ("epsilon="++show epsilon) $ go lambda ensL
                return $ (w,f_model'):(ensL')
                
-- instance (Eq modelparams, Semigroup model) => Semigroup (Ensemble (AdaBoostParams modelparams) model) where
--     
--     (<>) (Ensemble ens1 params1) (Ensemble ens2 params2) = Ensemble (map merge $ zip ens1 ens2) params'
--         where
--             merge ((w1,m1),(w2,m2)) = ((w1+w2)/2,m1<>m2)
--             params' = if params1/=params2
--                          then error "Ensemble.semigroup <>: different parameters"
--                          else params1

-- instance (Invertible model) => Invertible (Ensemble (AdaBoostParams modelparams) model) where
--     inverse ens = ens { ensembleL = map (\(w,m) -> (w,inverse m)) $ ensembleL ens }
