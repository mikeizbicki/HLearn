{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Classifiers.Ensemble.SemiBoost
    where

import Data.Semigroup

import HMine.Base
import HMine.Classifiers.Ensemble
import HMine.DataContainers
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- SemiBoostParams

data SemiBoostParams baseparams = SemiBoostParams
    { rounds :: Int
    , baseparams :: baseparams
    }
    
-------------------------------------------------------------------------------
-- Ensemble instances for Training

-- instance (WeightedBatchTrainer baseparams basemodel label) => BatchTrainerSS (SemiBoostParams baseparams) basemodel Bool where
instance 
    ( Classifier basemodel Bool
    , WeightedBatchTrainer baseparams basemodel Bool
    ) => 
    BatchTrainerSS (SemiBoostParams baseparams) 
                   (Ensemble (SemiBoostParams baseparams) basemodel Bool) 
                   Bool
    where

    trainBatchSS params@(SemiBoostParams rounds basemodel) lds_small uds = 
        go 1 (emptyEnsemble (getDataDesc lds) params)
        where
            lds = lds_small <> (fmap ((,) (error "SemiBoostParams.trainBatchSS: should never access uds labels before assignment")) uds)
              
            l = getNumObs lds
            u = getNumObs uds
            labeledL = [1..l]
            unlabeledL = [l+1..l+u]
            indexL = [1..l+u]
            isLabeled i = i<=l
            
            c=1
            similarity i j = 1

            go !itr !ens
                | itr > rounds = return ens
                | otherwise = do
                    let _Hint = map (bool2num . classify ens . snd) $ getDataL lds
                    let _y = map fst $ getDataL lds
                    
                    -- equation 9
                    let p = [ if isLabeled i
                                then        sum [ (similarity i j)*(exp $ (-2)*_Hi)*(indicator $ _yj==True ) | (j,_yj) <- zip labeledL _y]
                                else (c/2)*(sum [ (similarity i j)*(exp $ (_Hj-_Hi)) | (j,_Hj) <- zip indexL _Hint])
                            | (i,_Hi) <- zip indexL _Hint
                            ]
                            
                    -- equation 10
                    let q = [ if isLabeled i
                                then        sum [ (similarity i j)*(exp $ ( 2)*_Hi)*(indicator $ _yj==False) | (j,_yj) <- zip labeledL _y]
                                else (c/2)*(sum [ (similarity i j)*(exp $ (_Hi-_Hj)) | (j,_Hj) <- zip indexL _Hint])
                            | (i,_Hi) <- zip indexL _Hint
                            ]
                            
                    -- compute the new pseudo labels
                    let z = [ num2bool $ pi-qi | (pi,qi) <- zip p q ]
                    let lds' = fmap (\((l,dps),i,zi) -> if isLabeled i
                                    then (l,dps)
                                    else (zi,dps)
                                    )
                                $ zip3dsL lds indexL z
                        
                    -- find base learner
                    let _D = [ abs $ pi-qi | (pi,qi) <- zip p q ]
                        
                    model <- trainBatchW (baseparams params) $ zipdsL lds' _D
                        
                    -- compute step size
                    let _h = map (classify model . snd) $ getDataL lds
                    let err = (sum [pi*(indicator $ _hi==False) | (pi,_hi) <- zip p _h] 
                              +sum [qi*(indicator $ _hi==True ) | (qi,_hi) <- zip q _h])
                            / (sum [pi+qi | (pi,qi) <- zip p q])
                            
                    let alpha = (1/4)*(log $ (1-err)/err)
                    
                    -- iterate
                    let ens' = pushClassifier (alpha,model) ens
                    go (itr+1) ens'
