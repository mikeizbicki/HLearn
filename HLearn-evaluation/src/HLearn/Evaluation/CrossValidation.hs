{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module HLearn.Evaluation.CrossValidation
    where

import qualified Data.Map as Map
import qualified Data.Set as S
-- import qualified Data.Vector as V

import Debug.Trace
import Data.DList

import HLearn.Algebra
import HLearn.DataContainers
-- import HLearn.Evaluation.Metrics
import HLearn.Models.Classification
import HLearn.Models.Distributions
import qualified Control.ConstraintKinds as CK

-------------------------------------------------------------------------------
-- standard k-fold cross validation

-- data CrossValidation modelparams datapoint model (k :: Nat) where CrossValidation
--     { dataset :: 
--     }

crossValidation :: 
    ( HomTrainer modelparams datapoint model
    , Semigroup ret
    , Semigroup (container datapoint)
    , CK.Functor container
    , CK.FunctorConstraint container datapoint
    , CK.FunctorConstraint container model
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , CK.FoldableConstraint container datapoint
    , CK.Partitionable container
    , CK.PartitionableConstraint container datapoint
    ) => modelparams -> container datapoint -> (model -> container datapoint -> ret) -> Int -> ret
crossValidation modelparams dataset perfmeasure k = reduce $ do
    (testdata, trainingdata) <- genTestList datasetL
    let model = train' modelparams trainingdata
    let score = perfmeasure model testdata
    return score
    where
        datasetL = CK.partition k dataset

crossValidation_par modelparams dataset perfmeasure k = (parallel reduce) $ do
    (testdata, trainingdata) <- genTestList datasetL
    let model = train' modelparams trainingdata
    let score = perfmeasure model testdata
    return score
    where
        datasetL = CK.partition k dataset

crossValidation_monoid :: 
    ( HomTrainer modelparams datapoint model
    , Semigroup model
    , Semigroup ret
    , Semigroup (container datapoint)
    , CK.Functor container
    , CK.FunctorConstraint container datapoint
    , CK.FunctorConstraint container model
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , CK.FoldableConstraint container datapoint
    , CK.Partitionable container
    , CK.PartitionableConstraint container datapoint
    ) => modelparams -> container datapoint -> (model -> container datapoint -> ret) -> Int -> ret
crossValidation_monoid modelparams dataset perfmeasure k = reduce $ do
    (testdata,model) <- zip datasetL $ listAllBut modelL
    let score = perfmeasure model testdata
    return score
    where
        modelL = fmap (train' modelparams) datasetL
        datasetL = CK.partition k dataset

crossValidation_monoid_par modelparams dataset perfmeasure k = (parallel reduce) $ do
    (testdata,model) <- zip datasetL $ listAllBut modelL
    let score = perfmeasure model testdata
    return score
    where
        modelL = fmap (train' modelparams) datasetL
        datasetL = CK.partition k dataset


crossValidation_group :: 
    ( HomTrainer modelparams datapoint model
    , Group model
    , Semigroup ret
    , Semigroup (container datapoint)
    , CK.Functor container
    , CK.FunctorConstraint container datapoint
    , CK.FunctorConstraint container model
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , CK.FoldableConstraint container datapoint
    , CK.Partitionable container
    , CK.PartitionableConstraint container datapoint
    ) => modelparams -> container datapoint -> (model -> container datapoint -> ret) -> Int -> ret
crossValidation_group modelparams dataset perfmeasure k = reduce $ do
    testdata <- datasetL
    let model = fullModel <> (inverse $ train' modelparams testdata)
    let score = perfmeasure model testdata
    return score    
    where
        fullModel = reduce modelL
        modelL = fmap (train' modelparams) datasetL
        datasetL = CK.partition k dataset

crossValidation_group_par modelparams dataset perfmeasure k = (parallel reduce) $ do
    testdata <- datasetL
    let model = fullModel <> (inverse $ train' modelparams testdata)
    let score = perfmeasure model testdata
    return score    
    where
        fullModel = (parallel reduce) modelL
        modelL = fmap (train' modelparams) datasetL
        datasetL = CK.partition k dataset


listAllBut2 :: (Semigroup a) => [a] -> [a]
listAllBut2 !xs = [reduce $ testL i | i <- itrL]
    where
        itrL = [0..(length xs)-1]
        testL i = (take (i) xs) ++ (drop (i+1) xs)

listAllBut :: (Semigroup a) => [a] -> [a]
listAllBut !xs = [reduce $ testL i | i <- itrL]
    where
        itrL = [0..(length xs)-1]
        testL i = (fromList $ take (i) xs) `mappend` (fromList $ drop (i+1) xs)

instance CK.Foldable DList where
    foldr = Data.DList.foldr

genTestList :: (Semigroup a) => [a] -> [(a,a)]
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
--     ) => PerformanceMeasure Accuracy model container (LDPS label) (Gaussian Double)
--         where
--         
--     measure metricparams model testdata = train1dp ((foldl1 (+) checkedL)/(fromIntegral $ numdp) :: Double)
--         where
--             checkdp (label,dp) = indicator $ label==(classify model dp)
--             checkedL = CK.toList $ CK.fmap checkdp testdata
--             numdp = length $ CK.toList testdata

accuracy :: 
    ( CK.Functor container
    , CK.FunctorConstraint container Double
    , CK.FunctorConstraint container (label,datapoint)
    , CK.Foldable container
    , CK.FoldableConstraint container Double
    , CK.FoldableConstraint container [Double]
    , CK.FoldableConstraint container datapoint
    , CK.FoldableConstraint container (Labeled datapoint label)
    , CK.FoldableConstraint container [Labeled datapoint label]
    , Classifier model datapoint label
    , Eq label
    ) => model -> container (Labeled datapoint label) -> (Gaussian Double)
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
-- instance Semigroup CVResults

-- crossValidation :: 
--     ( NFData model
--     , NFData outtype
--     , Semigroup outtype
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
            