{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Evaluation.CrossValidation
    where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Vector as V
-- import qualified Data.Vector as V

import Debug.Trace

import qualified Data.DList as D

import HLearn.Algebra
-- import HLearn.Evaluation.Metrics
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import qualified Control.ConstraintKinds as CK

-------------------------------------------------------------------------------
-- standard k-fold cross validation

-- lame_crossvalidation :: 
--     ( LameTrainer container datapoint model
--     , Monoid ret
--     , Monoid (container datapoint)
--     , CK.Functor container
--     , CK.FunctorConstraint container datapoint
--     , CK.FunctorConstraint container model
--     , CK.Foldable container
--     , CK.FoldableConstraint container model
--     , CK.FoldableConstraint container datapoint
--     , CK.Partitionable container
--     , CK.PartitionableConstraint container datapoint
--     ) => container datapoint -> (model -> container datapoint -> ret) -> Int -> ret
-- lame_crossvalidation dataset perfmeasure k = reduce $ do
--     (testdata, trainingdata) <- genTestList datasetL
--     let model = lame_train trainingdata
--     let score = perfmeasure model testdata
--     return score
--     where
--         datasetL = CK.partition k dataset

-- | This is the standard cross-validation technique for use with the HomTrainer type class.  It is asymptotically faster than standard k-fold cross-validation (implemented with lame_crossvalidation), yet is guaranteed to get the exact same answer.
crossvalidation :: 
    ( HomTrainer model
    , Monoid ret
    , Monoid (container (Datapoint model))
    , CK.Partitionable container
    , CK.PartitionableConstraint container (Datapoint model)
    , F.Foldable container
    , Functor container
    ) => container (Datapoint model) -> (model -> container (Datapoint model) -> ret) -> Int -> ret
crossvalidation dataset perfmeasure k = reduce $ do
    (testdata,model) <- zip datasetL $ listAllBut modelL
    let score = perfmeasure model testdata
    return score
    where
        modelL = fmap train datasetL
        datasetL = CK.partition k dataset
    
-- crossvalidation_group :: 
--     ( HomTrainer modelparams datapoint model
--     , Group model
--     , Monoid ret
--     , CK.Functor container
--     , CK.FunctorConstraint container datapoint
--     , CK.FunctorConstraint container model
--     , CK.Foldable container
--     , CK.FoldableConstraint container model
--     , CK.FoldableConstraint container datapoint
--     , CK.Partitionable container
--     , CK.PartitionableConstraint container datapoint
--     , F.Foldable container
--     , Functor container
--     ) => modelparams -> container datapoint -> (model -> container datapoint -> ret) -> Int -> ret
-- crossvalidation_group modelparams dataset perfmeasure k = reduce $ do
--     testdata <- datasetL
--     let model = fullModel <> (inverse $ train' modelparams testdata)
--     let score = perfmeasure model testdata
--     return score    
--     where
--         fullModel = reduce modelL
--         modelL = fmap (train' modelparams) datasetL
--         datasetL = CK.partition k dataset
-- 
-- lame_crossvalidation dataset perfmeasure k = reduce $ do
--     (testdata, trainingdata) <- genTestList datasetL
--     let model = lame_train trainingdata
--     let score = perfmeasure model testdata
--     return score
--     where
--         datasetL = CK.partition k dataset

type LossFunction model = model -> [Datapoint model] -> Double

crossValidate :: (HomTrainer model, Eq (Datapoint model)) => 
    [[Datapoint model]] -> LossFunction model -> Normal Double
crossValidate xs f = train $ do
    testset <- xs
    let trainingset = concat $ filter (/=testset) xs
    let model = train trainingset
    return $ f model testset
    
crossValidate_monoid :: (HomTrainer model, Eq (Datapoint model)) => 
    [[Datapoint model]] -> LossFunction model -> Normal Double
crossValidate_monoid xs f = train $ do
    testset <- xs
    let trainingset = concat $ filter (/=testset) xs
    let model = train trainingset
    return $ f model testset    
    
crossValidate_group :: (HomTrainer model, Group model) => 
    [[Datapoint model]] -> LossFunction model -> Normal Double
crossValidate_group xs f = train $ do
    (testset,testModel) <- modelL
    let model = fullmodel <> inverse testModel
    return $ f model testset
    where
        modelL = zip xs $ map train xs
        fullmodel = reduce $ map snd modelL

    
-- crossValidation_group_par modelparams dataset perfmeasure k = (parallel reduce) $ do
--     testdata <- datasetL
--     let model = fullModel <> (inverse $ train' modelparams testdata)
--     let score = perfmeasure model testdata
--     return score    
--     where
--         fullModel = (parallel reduce) modelL
--         modelL = fmap (train' modelparams) datasetL
--         datasetL = CK.partition k dataset


listAllBut2 :: (Monoid a) => [a] -> [a]
listAllBut2 !xs = [reduce $ testL i | i <- itrL]
    where
        itrL = [0..(length xs)-1]
        testL i = (take (i) xs) ++ (drop (i+1) xs)

listAllBut :: (Monoid a) => [a] -> [a]
listAllBut !xs = [reduce $ testL i | i <- itrL]
    where
        itrL = [0..(length xs)-1]
        testL i = D.toList $ (D.fromList $ take (i) xs) `mappend` (D.fromList $ drop (i+1) xs)

genTestList :: (Monoid a) => [a] -> [(a,a)]
genTestList xs = zip xs $ listAllBut xs


-- class PerformanceMeasure measure model container datapoint outtype | measure -> outtype where
--     measure :: measure -> model -> container datapoint -> outtype
-- 
-- data Accuracy = Accuracy
-- 
-- instance
--     ( CK.Functor container
--     , CK.FunctorConstraint container Double
--     , CK.FunctorConstraint container (label,DPS)
--     , CK.Foldable container
--     , CK.FoldableConstraint container Double
--     , CK.FoldableConstraint container [Double]
--     , CK.FoldableConstraint container datapoint
--     , CK.FoldableConstraint container (LDPS label)
--     , CK.FoldableConstraint container [LDPS label]
--     , Classifier model DPS label
--     ) => PerformanceMeasure Accuracy model container (LDPS label) (Normal Double)
--         where
--         
--     measure metricparams model testdata = train1dp ((foldl1 (+) checkedL)/(fromIntegral $ numdp) :: Double)
--         where
--             checkdp (label,dp) = indicator $ label==(classify model dp)
--             checkedL = CK.toList $ CK.fmap checkdp testdata
--             numdp = length $ CK.toList testdata

type Labeled dp label = (label,dp)

accuracy :: 
    ( CK.Functor container
    , CK.FunctorConstraint container Double
    , CK.FunctorConstraint container (label,datapoint)
    , CK.Foldable container
    , CK.FoldableConstraint container Double
    , CK.FoldableConstraint container [Double]
    , CK.FoldableConstraint container datapoint
--     , CK.FoldableConstraint container (Labeled datapoint label)
--     , CK.FoldableConstraint container [Labeled datapoint label]
    , CK.FoldableConstraint container (Labeled datapoint label)
    , CK.FoldableConstraint container [Labeled datapoint label]
    , Classifier model
    , UnlabeledDatapoint model ~ datapoint 
    , Label model ~ label 
    , Eq label
    ) => model -> container (Labeled datapoint label) -> (Normal Double)
accuracy model testdata = train1dp ((foldl1 (+) checkedL)/(fromIntegral $ numdp) :: Double)
    where
        checkdp (label,dp) = indicator $ label==(classify model dp)
        checkedL = CK.toList $ CK.fmap checkdp testdata
        numdp = length $ CK.toList testdata


-- data CVType = CV_Normal | CV_Monoid | CV_Group
-- 
-- data CVParams = CVParams
--     { folds :: Int
--     , metric :: Metric metric model ds label outtype
--     }
-- 
-- data CVResults = CVResults
--     {
--     }
-- 
-- instance Model CVParams CVResults where
--     
-- instance HomTrainer CVParams (LDPS label) CVResults where
--     train' CVParams ldps = undefined
-- 
-- instance Monoid CVResults
-- 
-- instance Monoid CVResults

-- crossValidation :: 
--     ( NFData model
--     , NFData outtype
--     , Monoid outtype
--     , DataSparse label ds label
--     , DataSparse label ds DPS
--     , DataSparse label ds (Labeled DPS label)
--     , DataSparse label ds (Weighted (Labeled DPS label))
--     , DataSparse label ds [(label,probability)]
--     , HomTrainer modelparams model DPS label
--     , Metric metric model ds label outtype
--     ) =>
--      metric
--      -> modelparams
--      -> ds (Labeled DPS label)
--      -> Double
--      -> Double
--      -> Int
--      -> Random outtype
-- crossValidation metric modelparams inputdata trainingRatio labeledRatio rounds = foldl1' (<>) $
--     [ do
--         (trainingdata,testdata) <- randSplit trainingRatio inputdata
--         (lds,uds) <- randSplit labeledRatio trainingdata
--         let uds' = fmap snd uds
--         model <- trainBatchSS modelparams lds uds'
--         return $ deepseq model $ measure metric model testdata
--     | i <- [1..rounds]
--     ]
            
