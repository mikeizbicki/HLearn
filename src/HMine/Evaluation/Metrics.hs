{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module HMine.Evaluation.Metrics
    where
          
import Data.List
import Data.Number.LogFloat hiding (log)
          
import qualified Data.Map as Map
          
import HMine.Base
import HMine.Classifiers.TypeClasses
import HMine.Classifiers.Ensemble
import HMine.DataContainers
          
-------------------------------------------------------------------------------
-- ConfusionMatrix
          
newtype ConfusionMatrix label = ConfusionMatrix [(label,(Int,Int))]
    deriving (Show,Read,Eq)
    
genConfusionMatrix ::
    ( DataSparse label ds (LDPS label)
    , Classifier model label
    ) =>
    model -> ds (LDPS label) -> ConfusionMatrix label
genConfusionMatrix model lds = ConfusionMatrix . Map.toList $ Map.fromListWith addL $ map mergeL $ zip labelL labelL'
    where
        addL (a1,b1) (a2,b2) = (a1+a2,b1+b2)
        mergeL (l,l') = (l,(indicator $ l==l',indicator $ l/=l'))
        labelL  = map fst $ getDataL lds
        labelL' = map (classify model . snd) $ getDataL lds
        
-------------------------------------------------------------------------------
-- General Metrics

-- see: https://www.kaggle.com/wiki/MultiClassLogLoss
logloss :: 
    ( ProbabilityClassifier model label
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds label
    , DataSparse label ds [(label,Probability)]
    ) => 
    model -> ds (LDPS label) -> Double
logloss model testdata = logloss' testdata $ fmap (probabilityClassify model) (lds2uds testdata)
    where
        logloss' lds pds = -(1/(fromIntegral $ numLabels $ getDataDesc lds))*(foldl1' (+) zipL)
            where
                zipL = zipWith (\y (l,p) -> y*(log $ fromLogFloat p)) (concat $ y_ij) (concat $ getDataL pds)
                y_ij = fmap (\(l,dp) -> [ indicator $ l==j | j <- labelL $ getDataDesc lds ]) 
                            $ getDataL lds

-- accuracy = % correctly classified = 1-error rate
accuracy ::     
    ( Classifier model label
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds label
    ) => 
    model -> ds (LDPS label) -> Double
accuracy model testdata = (foldl1' (+) zipL)/(fromIntegral $ length zipL)
    where
        zipL = map (\(a,b) -> indicator $ a==b) $ zip (map fst $ getDataL testdata) (getDataL $ fmap (classify model) (lds2uds testdata))

-------------------------------------------------------------------------------
-- Ensemble Metrics

ensemblify :: (Ensemble modelparams model label -> t -> b) -> Ensemble modelparams model label -> t -> [b]
ensemblify metric ens testdata = map (\ens -> metric ens testdata) ensL
    where
        ensL = [ ens { ensembleL = drop i $ ensembleL ens } | i<-[0..length $ ensembleL ens]]
