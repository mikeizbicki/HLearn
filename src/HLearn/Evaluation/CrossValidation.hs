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

type LossFunction model = model -> [Datapoint model] -> Double

leaveOneOut :: [dp] -> [[dp]]
leaveOneOut xs = map (\x -> [x]) xs

folds :: Int -> [dp] -> [[dp]]
folds n xs = [map snd $ filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
    where
        ixs = addIndex 0 xs
        addIndex i [] = []
        addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)

errorRateTmp :: LossFunction model
errorRateTmp = undefined

errorRate ::
    ( Classifier model
    , Labeled (Datapoint model)
    , Eq (Label (Datapoint model))
    ) => LossFunction model
errorRate model dataL = (fromIntegral $ length $ filter (==True) resultsL) / (fromIntegral $ length dataL)
    where
        resultsL = map (\(l1,l2) -> l1/=l2) $ zip trueL classifyL
        trueL = map getLabel dataL
        classifyL = map (classify model . getAttributes) dataL

crossValidate :: (HomTrainer model, Eq (Datapoint model)) =>
    [[Datapoint model]] -> LossFunction model -> Normal Double Double
crossValidate xs f = train $ do
    testset <- xs
    let trainingset = concat $ filter (/=testset) xs
    let model = train trainingset
    return $ f model testset
    

cv_group :: (HomTrainer model, Group model) => 
    model -> [Datapoint model] -> (model -> [Datapoint model] -> Double) -> Normal Double Double
cv_group m dps f = train $ do
    dp <- dps
    let m' = sub1dp m dp
    return $ f m [dp]

crossValidate_group :: (HomTrainer model, Group model) =>
    [[Datapoint model]] -> LossFunction model -> Normal Double Double
crossValidate_group xs f = train $ do
    (testset,testModel) <- modelL
    let model = fullmodel <> inverse testModel
    return $ f model testset
    where
        modelL = zip xs $ map train xs
        fullmodel = reduce $ map snd modelL


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


