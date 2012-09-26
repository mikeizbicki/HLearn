{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Distributions.Categorical
    where

import Control.DeepSeq
import Data.List.Extras
import Data.Number.LogFloat
import Data.Semigroup
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Foldable as F

import HLearn.Base
import HLearn.DataContainers
import HLearn.Math.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- CategoricalParams

data CategoricalParams = CategoricalParams
    deriving (Read,Show,Eq)

instance NFData CategoricalParams where
    rnf x = ()

-------------------------------------------------------------------------------
-- Categorical

data Categorical label = Categorical 
        { pdfmap :: Map.Map label LogFloat
        , desc :: Maybe (DataDesc label)
        } 
    deriving (Show,Read,Eq)

instance (NFData label) => NFData (Categorical label) where
    rnf d = rnf $ pdfmap d

instance NFData LogFloat where
    rnf x = ()

-------------------------------------------------------------------------------
-- Distribution

instance (Ord label) => DistributionEstimator (Categorical label) (label,LogFloat) where
    {-# INLINE add1sample #-}
    add1sample dist (label,weight) = dist { pdfmap=Map.insertWith (+) label weight (pdfmap dist) }

instance (Ord label) => DistributionEstimator (Categorical label) (Weighted label) where
    {-# INLINE add1sample #-}
    add1sample dist (label,weight) = add1sample dist (label,logFloat weight)
    
instance (DistributionEstimator (Categorical label) (Weighted label)) => DistributionEstimator (Categorical label)  label where
    {-# INLINE add1sample #-}
    add1sample dist label = add1sample dist (label,1::Double)

instance (Ord label) => Distribution (Categorical label) label where

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
    cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist

    {-# INLINE mean #-}
    mean dist = fst $ argmax (snd) $ Map.toList $ pdfmap dist

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label) => Semigroup (Categorical label) where
    (<>) d1 d2 = if (desc d1)==(desc d2)
        then d1 {pdfmap=Map.unionWith (+) (pdfmap d1) (pdfmap d2)}
        else error "Categorical.(<>): different DataDesc"
    
instance (Ord label) => Monoid (Categorical label) where
    mempty = Categorical mempty Nothing
    mappend = (<>)

instance (Ord label) => Invertible (Categorical label) where
    inverse d1 = d1 {pdfmap=Map.map (0-) (pdfmap d1)}


-------------------------------------------------------------------------------
-- Helpers

-- list2dist :: (DistributionEstimator (Categorical label) (Weighted label), Semigroup (Categorical label)) => 
--     [(Probability,label)] -> Categorical label
list2dist xs = foldl1 (<>) $ map train1sample xs
-- list2dist xs = foldl1 (<>) $ map (\(l,p) -> train1sample (l,fromLogFloat p::Double)) xs
dist2list dist = {-map (\(l,d) -> (l,logFloat d)) $-} Map.toList $ pdfmap dist
