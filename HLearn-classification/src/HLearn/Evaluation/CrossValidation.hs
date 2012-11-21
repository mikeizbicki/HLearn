{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HLearn.Evaluation.CrossValidation
    where

import qualified Data.Map as Map
import qualified Data.Set as S
-- import qualified Data.Vector as V

import Control.Monad.Random
import Data.List
import Debug.Trace

import HLearn.Algebra
import HLearn.DataContainers
import HLearn.Evaluation.Metrics

-------------------------------------------------------------------------------
-- standard k-fold cross validation

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
            