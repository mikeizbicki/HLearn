{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Distributions.Categorical
    ( CategoricalParams(..)
    , Categorical (..)
    )
    where

import Control.Monad.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-- import HLearn.Base
-- import HLearn.DataContainers

-------------------------------------------------------------------------------
-- CategoricalParams

data CategoricalParams = CategoricalParams
    deriving (Read,Show,Eq)

instance NFData CategoricalParams where
    rnf x = ()

instance ModelParams CategoricalParams

-------------------------------------------------------------------------------
-- Categorical

data Categorical label = Categorical 
        { pdfmap :: Map.Map label Double
        } 
    deriving (Show,Read,Eq)

instance (NFData label) => NFData (Categorical label) where
    rnf d = rnf $ pdfmap d

instance (NFData label) => Model CategoricalParams (Categorical label) where
    params model = CategoricalParams

-------------------------------------------------------------------------------
-- Distribution

-- instance (NFData label) => WeightedSingletonTrainer CategoricalParams label (Categorical label) where
--     trainW params (label,weight) = Categorical $ Map.singleton label weight

-- instance (Label label, Semigroup [datapoint]) => Homomorphism Semigroup [datapoint] (Categorical label) trainW 

-- instance (NFData label) => Trainer CategoricalParams Int (Categorical Int) where
--     train params label = Categorical $ Map.singleton label 1
-- 
-- instance {-(Ord label, NFData label) => -}Trainer CategoricalParams [Int] (Categorical Int) where
--     train params xs = foldl' (<>) identity $ map (train params) xs
-- --     train params xs = foldl' train identity xs

instance (Ord label) => Distribution (Categorical label) label Double where

    {-# INLINE pdf #-}
    pdf dist label = 0.0001+(val/tot)
        where
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> 0
                Just x  -> x
            tot = F.foldl' (+) 0 $ pdfmap dist

    {-# INLINE cdf #-}
    cdf dist label = (Map.foldl' (+) 0 $ Map.filterWithKey (\k a -> k<=label) $ pdfmap dist) 
                   / (Map.foldl' (+) 0 $ pdfmap dist)
                   
    {-# INLINE cdfInverse #-}
    cdfInverse dist prob = go prob pdfL
        where
            pdfL = map (\k -> (k,pdf dist k)) $ Map.keys $ pdfmap dist
            go prob []     = fst $ last pdfL
            go prob (x:xs) = if prob < snd x && prob > (snd $ head xs)
                then fst x
                else go (prob-snd x) xs
--     cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist

    {-# INLINE mean #-}
    mean dist = fst $ argmax snd $ Map.toList $ pdfmap dist

    {-# INLINE drawSample #-}
    drawSample dist = do
        x <- getRandomR (0,1)
        return $ cdfInverse dist (x::Double)
        
-------------------------------------------------------------------------------
-- Algebra

instance (Ord label) => Semigroup (Categorical label) where
{-    (<>) d1 d2 = if (desc d1)==(desc d2)
        then d1 {pdfmap=Map.unionWith (+) (pdfmap d1) (pdfmap d2)}
        else error "Categorical.(<>): different DataDesc"-}
    (<>) d1 d2 = Categorical $ Map.unionWith (+) (pdfmap d1) (pdfmap d2)

instance HasIdentity (Categorical label) where
    identity = Categorical Map.empty

instance (Ord label) => Invertible (Categorical label) where
    inverse d1 = d1 {pdfmap=Map.map (0-) (pdfmap d1)}

instance (Ord label) => Monoid (Categorical label) where
    mempty = identity
    mappend = (<>)

-------------------------------------------------------------------------------
-- Helpers

-- list2dist :: (DistributionEstimator (Categorical label) (Weighted label), Semigroup (Categorical label)) => 
--     [(Probability,label)] -> Categorical label
-- list2dist xs = foldl1 (<>) $ map train1sample xs
-- list2dist xs = foldl1 (<>) $ map (\(l,p) -> train1sample (l,fromLogFloat p::Double)) xs
-- dist2list dist = {-map (\(l,d) -> (l,logFloat d)) $-} Map.toList $ pdfmap dist
