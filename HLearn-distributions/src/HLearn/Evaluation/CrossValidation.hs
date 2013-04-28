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
import Data.List.Split

import Debug.Trace
import qualified Data.DList as D

import HLearn.Algebra
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- standard k-fold cross validation

data FoldStrategy = LeaveOneOut | Folds Int

data CVParams model = CVParams
    { foldstrat :: FoldStrategy
    , lossfunction :: forall f. (Function f (Datapoint model) (Probability model)) => f
    }
    
-- crossvalidate [1..1000] (CVParams LeaveOneOut (Ave PDF) :: CVParams (Normal Double))
-- 
-- crossvalidation model xs loss k = (train :: [Double] -> Normal Double) $ crossvalidationL model xs loss k

-- | This is the standard cross-validation technique for use with the HomTrainer type class.  It is asymptotically faster than standard k-fold cross-validation (implemented with lame_crossvalidation), yet is guaranteed to get the exact same answer.
crossvalidation :: 
    ( HomTrainer model
    , Monoid (container (Datapoint model))
    , Partitionable container
    , F.Foldable container
    , Functor container
    ) => model                                          -- ^ specifies which model to use cross-validation for; may be undefined
      -> FoldStrategy                                   -- ^ number of folds
      -> (model -> container (Datapoint model) -> ret)  -- ^ loss function
      -> container (Datapoint model)                    -- ^ Data points
      -> [ret]                                          -- ^ return type
crossvalidation _model LeaveOneOut perfmeasure dataset = crossvalidation _model (Folds $ length $ F.toList dataset) perfmeasure dataset 
crossvalidation _model (Folds k)   perfmeasure dataset = do
    (testdata,model) <- zip datasetL $ listAllBut modelL
    let score = perfmeasure model testdata
    return score
    where
        modelL = fmap train datasetL
        datasetL = partition k dataset
    
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


pdfAve :: 
    ( Floating (Probability model)
    , F.Foldable container
    , PDF model
    , Functor container
    ) => model -> container (Datapoint model) -> Probability model
pdfAve dist xs = exp $ (F.foldl' (+) 0 $ fmap (log . pdf dist) xs) / (fromIntegral $ length $ F.toList xs)


            
class Partitionable t where
    partition :: Int -> t a -> [t a]

instance Partitionable [] where
    partition n xs = [map snd $ filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
        where
            ixs = addIndex 0 xs
            addIndex i [] = []
            addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)


-- instance Partitionable [] where
--     partition n xs = splitEvery (ceiling $ (fromIntegral $ length xs) / (fromIntegral n :: Double)) xs


instance Partitionable V.Vector where
    partition n vec = go 0
        where
            go i = if i>=V.length vec
                then []
                else (V.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= V.length vec
                        then (V.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ V.length vec) / (fromIntegral n)
