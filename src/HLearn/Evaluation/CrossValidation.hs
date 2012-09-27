{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module HLearn.Evaluation.CrossValidation
    where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as S
-- import qualified Data.Vector as V

import Control.Exception
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Debug.Trace
import System.IO
import System.IO.Unsafe

import HLearn.Base
import HLearn.DataContainers
import HLearn.Evaluation.Metrics
import HLearn.Math.TypeClasses
import HLearn.Models.Ensemble

-------------------------------------------------------------------------------

-- newtype Box inside = Box inside
-- 
-- instance (AverageableSemigroup Box

-- class (NFData a) => Averageable a where
--     ave :: [a] -> (a,a)
--     
-- instance Averageable Double where
--     ave = meanstddev
--     
-- instance Averageable [Double] where
--     ave xs = (map mean $ transpose xs, map stddev $ transpose xs)
-- 
-- meanstddev :: (Floating a) => [a] -> (a,a)
-- meanstddev xs = (mean xs, stddev xs)

-- newtype MetricBox = MetricBox
--     { ave :: Double
--     , stddev :: Double
--     , count :: Int
--     }
--           
-- instance Semigroup MetricBox where
--     (<>) m1 m2 = MetricBox
--         { ave=(ave m1 + ave m2)/(fi $ count m1 + count m2)
--         , stddev=(stddev m1 + stddev m2)/(fi $ count m1 + count m2)
--         , count= (count m1)+(count m2)
--         }


-------------------------------------------------------------------------------
-- standard k-fold cross validation

crossValidation :: 
    ( NFData model
    , NFData outtype
    , Semigroup outtype
    , DataSparse label ds label
    , DataSparse label ds DPS
    , DataSparse label ds (Labeled DPS label)
    , DataSparse label ds (Weighted (Labeled DPS label))
    , DataSparse label ds [(label,Probability)]
    , BatchTrainerSS modelparams model DPS label
    , Metric metric model ds label outtype
    ) =>
     metric
     -> modelparams
     -> ds (Labeled DPS label)
     -> Double
     -> Double
     -> Int
     -> HLearn outtype
crossValidation metric modelparams inputdata trainingRatio labeledRatio rounds = foldl1' (<>) $
    [ do
        (trainingdata,testdata) <- randSplit trainingRatio inputdata
        (lds,uds) <- randSplit labeledRatio trainingdata
        let uds' = fmap snd uds
        model <- trainBatchSS modelparams lds uds'
        return $ deepseq model $ measure metric model testdata
    | i <- [1..rounds]
    ]

-------------------------------------------------------------------------------
-- Monoidal cross validation

{-crossValidation_monoidal ::
    ( MutableTrainer modelparams model modelST label
    , ProbabilityClassifier model DPS label
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
        ldsL = splitds folds lds
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
        modelItr modelL itr = mconcat $ map snd $ filter ((/=) itr . fst) $ zip [0..] modelL-}
            