{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Classifiers.Ensemble.SemiBoost
    where

import Control.DeepSeq
import Data.Array
import Data.Semigroup
import Debug.Trace

import HMine.Base
import HMine.Classifiers.Ensemble
import HMine.Classifiers.KNN
import HMine.DataContainers
import HMine.Evaluation.Metrics
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- SemiBoostParams

data SemiBoostParams baseparams = SemiBoostParams
    { rounds :: Int
    , baseparams :: baseparams
    }
    
instance (NFData baseparams) => NFData (SemiBoostParams baseparams) where
    rnf params = deepseq rounds $ rnf baseparams
    
-------------------------------------------------------------------------------
-- Ensemble instances for Training

-- instance (WeightedBatchTrainer baseparams basemodel label) => BatchTrainerSS (SemiBoostParams baseparams) basemodel Bool where
instance 
    ( Classifier basemodel DPS Bool
    , WeightedBatchTrainer baseparams basemodel DPS Bool
    ) => 
    BatchTrainerSS (SemiBoostParams baseparams) 
                   (Ensemble (SemiBoostParams baseparams) basemodel Bool) 
                   DPS
                   Bool
    where

    trainBatchSS params@(SemiBoostParams rounds basemodel) lds_small uds = do
--         model <- trainBatch defKNNParams lds_small
--         let lds = lds_small <> (fmap (\dp -> (classify model dp,dp)) uds)
        go 1 (emptyEnsemble (getDataDesc lds) params)
        where
--             lds = lds_small <> (fmap ((,) (error "SemiBoostParams.trainBatchSS: should never access uds labels before assignment")) uds)
            lds = lds_small <> (fmap ((,) True) uds)
            
            l = getNumObs lds_small
            u = getNumObs uds
            labeledL = [1..l]
            unlabeledL = [l+1..l+u]
            indexL = [1..l+u]
            isLabeled i = i<=l
            dpL = getDataL $ (fmap snd lds_small) <> uds
            
            c=0
            similarity i j = 1
--             similarity i j = similarityDPS (dpL !! (i-1)) (dpL !! (j-1))
{-            similarity i j = sim_arr ! (i,j)
            sim_arr = listArray ((1,1),(l+u,l+u))
                        [ similarityDPS dpi dpj
                        | dpi <- dpL
                        , dpj <- dpL
                        ]-}
            
            go !itr !ens
                | itr > rounds = return ens
                | otherwise = do
                    let _Hint = map (bool2num . classify ens . snd) $ getDataL lds
                    let _y = map fst $ getDataL lds_small
                    
                    -- equation 9
                    let pL = [ (if isLabeled i
                                then sum [ (similarity i j)*(exp $ (-2)*_Hi)*(indicator $ _yj==True ) | (j,_yj) <- zip labeledL _y]
                                else 0
                               )
--                               +(c/2)*(sum [ (similarity i j)*(exp $ (_Hj-_Hi)) | (j,_Hj) <- zip indexL _Hint])
                             | (i,_Hi) <- zip indexL _Hint
                             ]
                            
                    -- equation 10
                    let qL = [ (if isLabeled i
                                then sum [ (similarity i j)*(exp $ ( 2)*_Hi)*(indicator $ _yj==False) | (j,_yj) <- zip labeledL _y]
                                else 0
                               ) 
--                               +(c/2)*(sum [ (similarity i j)*(exp $ (_Hi-_Hj)) | (j,_Hj) <- zip indexL _Hint])
                             | (i,_Hi) <- zip indexL _Hint
                             ]
                            
                    -- compute the new pseudo labels
                    let zL = [ num2bool $ pi-qi | (pi,qi) <- zip pL qL ]
                    let lds' = fmap (\((l,dps),i,zi) -> if isLabeled i
                                    then (l,dps)
                                    else (zi,dps)
                                    )
                                $ zip3dsL lds indexL zL
                        
                    -- find base learner
                    let _D = [ abs $ pi-qi | (pi,qi) <- zip pL qL ]
                    model <- trainBatchW (baseparams params) $ zipdsL lds' _D
                        
                    -- compute step size
                    let _h = map (classify model . snd) $ getDataL lds
                    let err = (sum [pi*(indicator $ _hi==False) | (pi,_hi) <- zip pL _h] 
                              +sum [qi*(indicator $ _hi==True ) | (qi,_hi) <- zip qL _h])
                            / (sum [pi+qi | (pi,qi) <- zip pL qL])
                            
                    let alpha = (1/4)*(log $ (1-err)/err)
                    
                    -- iterate
                    let ens' = pushClassifier (alpha,model) ens
                    trace ("itr="++show itr++", l/u="++show l++"/"++show u++", accuracy="++show (accuracy ens' lds)++", alpha="++show alpha++", err="++show err)
                        $ go (itr+1) ens'

similarityDPS :: DPS -> DPS -> Double
similarityDPS dp1 dp2 = similarityDPF (dps2dpf len dp1) (dps2dpf len dp2)
    where
        len = max (fst $ last dp1) (fst $ last dp2)

similarityDPF :: DPF -> DPF -> Double
similarityDPF dp1 dp2 = (l2norm) {-/ (sigma^2)-}
    where
        l2norm = sqrt $ sum $ map sub2 $ zip dp1 dp2
        sub2 (di1,di2) = 
            case di1 of
                Continuous x -> case di2 of
                    Continuous y -> (x - y)^2/sigma
                    Discrete y -> error "similarity: x continuous, y discrete"
                    Missing -> 1
                Discrete x -> case di2 of
                    Continuous y -> error "similarityDPF: x discrete, y continuous"
                    Discrete y -> 1
                    Missing -> 1
                Missing -> case di2 of
                    Missing -> 0
                    otherwise -> 1
        sigma = 1
