{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module HMine.Testing
    where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as S
-- import qualified Data.Vector as V

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Binary
import Data.List
import Data.Monoid
import Data.Number.LogFloat hiding (log)
import Debug.Trace
import GHC.Arr
import System.Random
import System.IO
import System.IO.Unsafe

import HMine.Base
import HMine.DataContainers
import HMine.Classifiers.TypeClasses
import HMine.Classifiers.Ensemble
import HMine.MiscUtils

-------------------------------------------------------------------------------

meanstddev :: [Double] -> (Double,Double)
meanstddev xs = (mean xs, stddev xs)

-------------------------------------------------------------------------------
-- Monoidal cross validation

crossValidation_monoidal ::
    ( MutableTrainer modelparams model modelST label
    , ProbabilityClassifier model label
    , Monoid model
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds [(label,Probability)]
    , DataSparse label ds label
    , NFData model
    ) =>
    modelparams -> ds (LDPS label) -> Int -> [Double]
crossValidation_monoidal modelparams lds folds = [logloss (modelItr modelL i) (testdata ldsL i) | i<-[0..folds-1]]
    where
        foldLen = ceiling $ (fromIntegral $ getNumObs lds)/(fromIntegral folds)
        ldsL = splitds foldLen lds
        modelL = map (trainST modelparams) ldsL
        
{-        testdata :: 
            ( DataSparse label ds (LDPS label)
            , DataSparse label ds (UDPS label)
            , DataSparse label ds [(label,Probability)]
            ) => 
            Int -> ds (LDPS label)-}
        testdata :: [a] -> Int -> a
        testdata ldsL itr = snd . head $ filter ((==) itr . fst) $ zip [0..] ldsL
--         testdata ldsL itr = ldsL !! itr

        modelItr :: (Monoid model) => [model] -> Int -> model
        modelItr modelL itr = mconcat $ map snd $ filter ((/=) itr . fst) $ zip [0..] modelL
        
-------------------------------------------------------------------------------
-- cross validation

crossValidation :: 
    ( NFData model
--     , MutableTrainer modelparams model modelST label
    , BatchTrainer modelparams model label
    , {-Probability-}Classifier model label
    , DataSparse label ds (WLDPS label)
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds [(label,Probability)]
    , DataSparse label ds label
    ) => 
    modelparams -> ds (LDPS label) -> Double -> Int -> HMine (Double,Double)
crossValidation modelparams trainingdata trainingRatio rounds = do
    res <- sequence [ do
            (trainingdata,testdata) <- randSplit trainingRatio trainingdata
            ret <- runTest modelparams trainingdata testdata
            return $ deepseq ret ret
        | i <- [1..rounds]
        ]
    return $ meanstddev res

runTest :: 
    ( NFData model
--     , MutableTrainer modelparams model modelST label
    , BatchTrainer modelparams model label
    , {-Probability-}Classifier model label
    , DataSparse label lds (WLDPS label)
    , DataSparse label lds (LDPS label)
    , DataSparse label lds (UDPS label)
    , DataSparse label lds [(label,Probability)]
    , DataSparse label lds label
--     , DataSparse label uds (UDPS label)
    ) => 
    modelparams -> lds (LDPS label) -> lds (LDPS label) -> HMine Double
runTest modelparams trainingdata testdata = do
    model <- trainBatch modelparams trainingdata
    return $ deepseq model $ accuracy model testdata
--     return $ deepseq model $ logloss model testdata

-------------------------------------------------------------------------------
-- Ensemble-specific

testEnsemble :: 
    ( NFData modelparams, NFData model, NFData label
    , Classifier model label
    , BatchTrainer ensparams (Ensemble modelparams model label) label
    , DataSparse label ds (WLDPS label)
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds [(label,Probability)]
    , DataSparse label ds label
    ) => 
    ensparams -> ds (LDPS label) -> Double -> Int -> HMine [(Double,Double)]
testEnsemble modelparams trainingdata trainingRatio rounds = do
    res <- sequence [ do
            (trainingdata,testdata) <- randSplit trainingRatio trainingdata
            ret <- runTestEnsemble modelparams trainingdata testdata
            return $ deepseq ret ret
        | i <- [1..rounds]
        ]
    return $ map meanstddev $ transpose res

runTestEnsemble :: 
    ( NFData modelparams, NFData model, NFData label
    , BatchTrainer ensparams (Ensemble modelparams model label) label
    , {-Probability-}Classifier model label
    , DataSparse label lds (WLDPS label)
    , DataSparse label lds (LDPS label)
    , DataSparse label lds (UDPS label)
    , DataSparse label lds [(label,Probability)]
    , DataSparse label lds label
    ) => 
    ensparams -> lds (LDPS label) -> lds (LDPS label) -> HMine [Double]
runTestEnsemble modelparams trainingdata testdata = do
    model <- trainBatch modelparams trainingdata
    return $ deepseq model $ ensembleAccuracy model testdata

ensembleAccuracy ens testdata = map (\ens -> accuracy ens testdata) ensL
    where
        ensL = [ ens {ensembleL = drop i $ ensembleL ens} | i<-[0..length $ ensembleL ens]]

-------------------------------------------------------------------------------
-- measurement functions

-- see: https://www.kaggle.com/wiki/MultiClassLogLoss
logloss :: 
    ( ProbabilityClassifier model label
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds [(label,Probability)]
    ) => 
    model -> ds (LDPS label) -> Double
-- scoreModel model testdata = logloss testdata $ map (probabilityClassify model) (map snd $ ld testdata)
logloss model testdata = logloss' testdata $ fmap (probabilityClassify model) (lds2uds testdata)
    where
        logloss' lds pds = -(1/(fromIntegral $ numLabels $ getDataDesc lds))*(foldl1' (+) zipL)
            where
                zipL = zipWith (\y (l,p) -> y*(log $ fromLogFloat p)) (concat $ y_ij) (concat $ getDataL pds)
                y_ij = fmap (\(l,dp) -> [ indicator $ l==j | j <- labelL $ getDataDesc lds ]) 
                            $ getDataL lds

-- 1-error rate
accuracy :: 
    ( Classifier model label
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds label
    ) => 
    model -> ds (LDPS label) -> Double
-- scoreModel model testdata = logloss testdata $ map (probabilityClassify model) (map snd $ ld testdata)
accuracy model testdata = (foldl1' (+) zipL)/(fromIntegral $ length zipL)
    where
        zipL = map (\(a,b) -> indicator $ a==b) $ zip (map fst $ getDataL testdata) (getDataL $ fmap (classify model) (lds2uds testdata))
            