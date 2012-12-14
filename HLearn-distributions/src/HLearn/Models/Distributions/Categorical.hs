{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module HLearn.Models.Distributions.Categorical
    ( Categorical (..)
    , CategoricalParams(..)
    , dist2list
    , mostLikely
    )
    where

import Control.Monad.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- CategoricalParams

-- | The Categorical distribution takes no parameters
data CategoricalParams = CategoricalParams
    deriving (Read,Show,Eq)

instance NFData CategoricalParams where
    rnf x = ()

instance Model CategoricalParams (Categorical label probtype) where
    getparams model = CategoricalParams

instance DefaultModel CategoricalParams (Categorical label probtype) where
    defparams = CategoricalParams

-------------------------------------------------------------------------------
-- Categorical

data Categorical sampletype probtype = Categorical 
        { pdfmap :: !(Map.Map sampletype probtype)
        } 
    deriving (Show,Read,Eq)

dist2list :: Categorical sampletype probtype -> [(sampletype,probtype)]
dist2list (Categorical pdfmap) = Map.toList pdfmap

instance (NFData sampletype, NFData probtype) => NFData (Categorical sampletype probtype) where
    rnf d = rnf $ pdfmap d

-------------------------------------------------------------------------------
-- Training

instance (Ord label, Num probtype) => HomTrainer CategoricalParams (label,probtype) (Categorical label probtype) where
    train1dp' params (dp,w) = Categorical $ Map.singleton dp w

instance (Ord label, Num probtype) => HomTrainer CategoricalParams label (Categorical label probtype) where
    train1dp' params dp = Categorical $ Map.singleton dp 1

-------------------------------------------------------------------------------
-- Distribution

instance (Ord label, Ord prob, Floating prob, Random prob) => Distribution (Categorical label prob) label prob where

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

{-    {-# INLINE mean #-}
    mean dist = fst $ argmax snd $ Map.toList $ pdfmap dist

    {-# INLINE drawSample #-}
    drawSample dist = do
        x <- getRandomR (0,1)
        return $ cdfInverse dist (x::prob)
-}

mostLikely :: Ord prob => Categorical label prob -> label
mostLikely dist = fst $ argmax snd $ Map.toList $ pdfmap dist


-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, Num probtype{-, NFData probtype-}) => Semigroup (Categorical label probtype) where
    (<>) !d1 !d2 = {-deepseq res $-} Categorical $ res
        where
            res = Map.unionWith (+) (pdfmap d1) (pdfmap d2)

instance (Ord label, Num probtype) => RegularSemigroup (Categorical label probtype) where
    inverse d1 = d1 {pdfmap=Map.map (0-) (pdfmap d1)}

instance (Ord label, Num probtype) => Monoid (Categorical label probtype) where
    mempty = Categorical Map.empty
    mappend = (<>)

instance (Ord label, Num probtype) => Group (Categorical label probtype)