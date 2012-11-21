{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module HLearn.Evaluation.Metrics
    where
          
import Data.List
          
import qualified Data.Map as Map
          
import HLearn.Algebra
import HLearn.DataContainers
import HLearn.Models.Classification
import HLearn.Models.Distributions
                 
-------------------------------------------------------------------------------
-- ConfusionMatrix
          
-- data ConfusionMatrix

newtype CM label = CM [(label,(Int,Int))]
    deriving (Show,Read,Eq)
    
{-instance (Classifier model DPS label) => Metric ConfusionMatrix model (CM label) where
    measure metricparams model testdata = CM . Map.toList $ Map.fromListWith addL $ map mergeL $ zip labelL labelL'
        where
            addL (a1,b1) (a2,b2) = (a1+a2,b1+b2)
            mergeL (l,l') = (l,(indicator $ l==l',indicator $ l/=l'))
            labelL  = map fst $ getDataL testdata
            labelL' = map (classify model . snd) $ getDataL testdata-}
    
genConfusionMatrix ::
    ( DataSparse label ds (Labeled datatype label)
    , Classifier model datatype label
    ) =>
    model -> ds (Labeled datatype label) -> CM label
genConfusionMatrix model lds = CM . Map.toList $ Map.fromListWith addL $ map mergeL $ zip labelL labelL'
    where
        addL (a1,b1) (a2,b2) = (a1+a2,b1+b2)
        mergeL (l,l') = (l,(indicator $ l==l',indicator $ l/=l'))
        labelL  = map fst $ getDataL lds
        labelL' = map (classify model . snd) $ getDataL lds
        
-------------------------------------------------------------------------------
-- General Metrics

class Metric metricparams model ds label outtype | metricparams -> outtype where
    measure::     
        metricparams -> model -> ds (LDPS label) -> outtype

data Accuracy = Accuracy

instance( Classifier model DPS label
        , DataSparse label ds (LDPS label)
        , DataSparse label ds (UDPS label)
        , DataSparse label ds label
        ) => 
    Metric Accuracy model ds label (Gaussian Double)
        where
        
    measure metricparams model testdata = train1dp ((foldl1' (+) zipL)/(fromIntegral $ length zipL) :: Double)
        where
            zipL = map (\(a,b) -> indicator $ a==b) $ zip (map fst $ getDataL testdata) (getDataL $ fmap (classify model) (lds2uds testdata))


data LogLoss = LogLoss

instance
    ( Classifier model DPS label
    , ProbabilityClassifier model DPS label
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds label
    , DataSparse label ds [(label,Double)]
    ) => 
    Metric LogLoss model ds label (Gaussian Double)
        where
              
--     measure metricparams model testdata = 
--         train1dp $ (logloss' testdata $ fmap (dist2list . probabilityClassify model) (lds2uds testdata) :: Double)
--         where
--             logloss' lds pds = -(1/(fromIntegral $ numLabels $ getDataDesc lds))*(foldl1' (+) zipL)
--                 where
--                     zipL = zipWith (\y (l,p) -> y*(log $ fromLogFloat p)) (concat $ y_ij) (concat $ getDataL pds)
--                     y_ij = fmap (\(l,dp) -> [ indicator $ l==j | j <- labelL $ getDataDesc lds ]) 
--                                 $ getDataL lds

-------------------------------------------------------------------------------
-- Ensemble Metrics

-- data EnsembleSteps basemetric = EnsembleSteps basemetric
-- 
-- instance 
--     ( Metric basemetric (Ensemble baseparams basemodel label) ds label outtype
--     ) => Metric (EnsembleSteps basemetric) (Ensemble baseparams basemodel label) ds label [outtype] where
--     measure (EnsembleSteps basemetric) ens testdata = map (\ens -> measure basemetric ens testdata) ensL
--         where
--             ensL = [ ens { ensembleL = drop i $ ensembleL ens } | i<-[0..length $ ensembleL ens]]
