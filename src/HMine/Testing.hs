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
import Control.Monad.Random
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
import HMine.MiscUtils

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
--     , RandomGen g
--     , DataSparse ds Int
    , DataSparse label ds (WDPS label)
    , DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    , DataSparse label ds [(label,Probability)]
    , DataSparse label ds label
    ) => 
    modelparams -> ds (LDPS label) -> Double -> Int -> HMine [Double]
crossValidation modelparams trainingdata trainingRatio rounds = 
    sequence [ do
            (trainingdata,testdata) <- randSplit trainingRatio trainingdata
            ret <- runTest modelparams trainingdata testdata
            return $ deepseq ret ret
        | i <- [1..rounds]
        ]

runTest :: 
    ( NFData model
--     , MutableTrainer modelparams model modelST label
    , BatchTrainer modelparams model label
    , {-Probability-}Classifier model label
    , DataSparse label lds (WDPS label)
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

-- see: https://www.kaggle.com/wiki/MultiClassLogLoss
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
            

{-logloss :: 
    ( DataSparse label lds (LDPS label)
    , DataSparse label pds [(label,Probability)]
    ) => 
    lds (LDPS label) -> pds [(label,Probability)] -> Double-}
        
        
-- logloss lds results = -(1/(fromIntegral $ length labelL))*(sum [(y i j)*(log $ p i j) | i<-obsL, j<-labelL])
--     where
--         obsL = getObsL lds
--         labelL = getLabelL lds
--         y i j = 
--             if (fst $ (ld testdata) !! i)==j
--                then 1
--                else 0
--         p :: Int -> Int -> Double
--         p i j = 
--             case lookup j (results !! i) of
--                 Just x -> safeProb $ fromLogFloat x
--                 Nothing -> error "logloss: y i j; label index (j) not provided"
--                 
--         safeProb x = 
--             if x==0
--                then 1e-15
--                else x


