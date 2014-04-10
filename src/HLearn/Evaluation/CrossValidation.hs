{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}
module HLearn.Evaluation.CrossValidation
    where

import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Control.Monad.Trans
import Data.Array.ST
import GHC.Arr
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Vector as V
-- import qualified Data.Vector as V

import Debug.Trace

import qualified Data.DList as D

import HLearn.Algebra
import HLearn.Algebra.History
-- import HLearn.Evaluation.Metrics
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import qualified Control.ConstraintKinds as CK

-------------------------------------------------------------------------------
-- standard k-fold cross validation

type SamplingMethod = forall dp r. (MonadRandom r) => [dp] -> r [[dp]]

leaveOneOut :: SamplingMethod 
leaveOneOut xs = return $ map (\x -> [x]) xs

withPercent :: Double -> SamplingMethod -> SamplingMethod
withPercent p f xs = do 
    xs' <- shuffle xs
    f $ take (floor $ (fromIntegral $ length xs') * p) xs'

repeatExperiment :: Int -> SamplingMethod -> SamplingMethod
repeatExperiment i f xs = do
    liftM concat $ forM [1..i] $ \i -> do
        f xs

kfold :: Int -> SamplingMethod
kfold k xs = do
    xs' <- shuffle xs
    return [takeEvery k $ drop j xs' | j<-[0..k-1]]
    where
        takeEvery n [] = []
        takeEvery n xs = head xs : (takeEvery n $ drop n xs)

numSamples :: Int -> SamplingMethod -> SamplingMethod
numSamples n f dps = f $ take n dps

-- | randomly shuffles a list in time O(n log n); see http://www.haskell.org/haskellwiki/Random_shuffle
-- shuffle :: (MonadRandom (m g), RandomGen g) => [a] -> m g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `liftM` getRandomRs (0, l-1)
    let ar = runSTArray $ do
            ar <- thawSTArray $ listArray (0, l-1) xs
            forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
                vi <- readSTArray ar i
                vj <- readSTArray ar j
                writeSTArray ar j vi
                writeSTArray ar i vj
            return ar
    return (elems ar)

---------------------------------------

type LossFunction = forall model. 
    ( Classifier model
    , HomTrainer model 
    , Labeled (Datapoint model)
    , Eq (Label (Datapoint model))
    ) => model -> [Datapoint model] -> Double

accuracy :: LossFunction 
accuracy model dataL = (fromIntegral $ length $ filter (==False) resultsL) / (fromIntegral $ length dataL)
    where
        resultsL = map (\(l1,l2) -> l1/=l2) $ zip trueL classifyL
        trueL = map getLabel dataL
        classifyL = map (classify model . getAttributes) dataL

errorRate :: LossFunction
errorRate model dataL = 1 - accuracy model dataL

---------------------------------------


validateM :: forall model g container.
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([Datapoint model] -> History model)
      -> RandT g History (Normal Double Double)
validateM genfolds loss xs _train = do
    xs' <- genfolds $ F.toList xs 
    lift $ collectEvents $ fmap train $ forM xs' $ \testset -> do
        let trainingset = concat $ filter (/=testset) xs'
        model <- _train trainingset
        return $ loss model testset

validate_monoidM :: 
    ( HomTrainer model
    , Classifier model
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([Datapoint model] -> model)
      -> (model -> model -> model)
      -> History (Normal Double Double)
validate_monoidM genfolds loss xs _train _mappend = do
    xs' <- genfolds $ F.toList xs
    let ms = map _train xs' -- :: [model]
        prefix = Nothing : map Just (init $ scanl1 _mappend ms) -- :: [Maybe model]
        suffix = map Just (tail $ scanr1 _mappend ms) ++ [Nothing] -- :: [Maybe model]

    let go (dps,Nothing,Just s) = loss s dps
        go (dps,Just p,Nothing) = loss p dps
        go (dps,Just p,Just s) = loss (_mappend p s) dps
        resL = map go (zip3 xs' prefix suffix)

    let res = train resL :: Normal Double Double
    return res

-------------------

validate :: 
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([Datapoint model] -> model)
      -> Rand g (Normal Double Double)
validate genfolds loss xs _train = do
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        let model = _train trainingset 
        return $ loss model testset

validate_monoid :: 
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([Datapoint model] -> model)
      -> (model -> model -> model)
      -> Rand g (Normal Double Double)
validate_monoid genfolds loss xs _train _mappend = do
    xs' <- genfolds $ F.toList xs
    let ms = map _train xs' -- :: [model]
        prefix = Nothing : map Just (init $ scanl1 _mappend ms) -- :: [Maybe model]
        suffix = map Just (tail $ scanr1 _mappend ms) ++ [Nothing] -- :: [Maybe model]

    let go (dps,Nothing,Just s) = loss s dps
        go (dps,Just p,Nothing) = loss p dps
        go (dps,Just p,Just s) = loss (_mappend p s) dps
        resL = map go (zip3 xs' prefix suffix)

    let res = train resL :: Normal Double Double
    return res

crossValidate :: 
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> model 
      -> Rand g (Normal Double Double)
crossValidate genfolds loss xs _model = do
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        let model = train trainingset `asTypeOf` _model
        return $ loss model testset


crossValidate_group :: 
    ( HomTrainer model
    , Classifier model
    , Abelian model
    , Group model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> model 
      -> Rand g (Normal Double Double)
crossValidate_group genfolds loss xs _model = do
    let m = train xs `asTypeOf` _model
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let model = m `subBatch` testset
        return $ loss model testset

cv_group :: (HomTrainer model, Group model) => 
    model -> [Datapoint model] -> (model -> [Datapoint model] -> Double) -> Normal Double Double
cv_group m dps f = train $ do
    dp <- dps
    let m' = sub1dp m dp
    return $ f m [dp]

-- crossValidate_group :: (HomTrainer model, Group model) =>
--     [[Datapoint model]] -> LossFunction -> Normal Double Double
-- --     [[Datapoint model]] -> LossFunction model -> Normal Double Double
-- crossValidate_group xs f = train $ do
--     (testset,testModel) <- modelL
--     let model = fullmodel <> inverse testModel
--     return $ f model testset
--     where
--         modelL = zip xs $ map train xs
--         fullmodel = reduce $ map snd modelL


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


