{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}


-- | The categorical distribution is used for discrete data.  It is also sometimes called the discrete distribution or the multinomial distribution.  For more, see the wikipedia entry: <https://en.wikipedia.org/wiki/Categorical_distribution>
module HLearn.Models.Distributions.Univariate.Categorical
    ( 
    -- * Data types
    Categorical (Categorical)
    
    -- * Helper functions
    , dist2list
    , mostLikely
    )
    where

import Control.DeepSeq
import Control.Monad.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Categorical


newtype Categorical sampletype prob = Categorical 
    { pdfmap :: !(Map.Map sampletype prob)
    } 
    deriving (Show,Read,Eq,Ord)

instance (NFData sampletype, NFData prob) => NFData (Categorical sampletype prob) where
    rnf d = rnf $ pdfmap d

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, Num prob) => Abelian (Categorical label prob)
instance (Ord label, Num prob) => Monoid (Categorical label prob) where
    mempty = Categorical Map.empty
    mappend !d1 !d2 = Categorical $ res
        where
            res = Map.unionWith (+) (pdfmap d1) (pdfmap d2)

instance (Ord label, Num prob) => Group (Categorical label prob) where
    inverse d1 = d1 {pdfmap=Map.map (0-) (pdfmap d1)}

instance (Num prob) => HasRing (Categorical label prob) where
    type Ring (Categorical label prob) = prob
instance (Ord label, Num prob) => Module (Categorical label prob) where
    p .* (Categorical pdf) = Categorical $ Map.map (*p) pdf

-------------------------------------------------------------------------------
-- Training

instance (Ord label, Num prob) => HomTrainer (Categorical label prob) where
    type Datapoint (Categorical label prob) = label
    train1dp dp = Categorical $ Map.singleton dp 1

instance (Num prob) => NumDP (Categorical label prob) where
    numdp dist = F.foldl' (+) 0 $ pdfmap dist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (Categorical label prob) where
    type Probability (Categorical label prob) = prob

instance (Ord label, Ord prob, Fractional prob) => PDF (Categorical label prob) where

    {-# INLINE pdf #-}
    pdf dist label = {-0.0001+-}(val/tot)
        where
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> 0
                Just x  -> x
            tot = F.foldl' (+) 0 $ pdfmap dist

instance (Ord label, Ord prob, Fractional prob) => CDF (Categorical label prob) where

    {-# INLINE cdf #-}
    cdf dist label = (Map.foldl' (+) 0 $ Map.filterWithKey (\k a -> k<=label) $ pdfmap dist) 
                   / (Map.foldl' (+) 0 $ pdfmap dist)
                   
    {-# INLINE cdfInverse #-}
    cdfInverse dist prob = go cdfL
        where
            cdfL = sortBy (\(k1,p1) (k2,p2) -> compare p2 p1) $ map (\k -> (k,pdf dist k)) $ Map.keys $ pdfmap dist
            go (x:[]) = fst $ last cdfL
            go (x:xs) = if prob < snd x -- && prob > (snd $ head xs)
                then fst x
                else go xs
--     cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist

--     {-# INLINE mean #-}
--     mean dist = fst $ argmax snd $ Map.toList $ pdfmap dist
-- 
--     {-# INLINE drawSample #-}
--     drawSample dist = do
--         x <- getRandomR (0,1)
--         return $ cdfInverse dist (x::prob)


instance (Num prob, Ord prob, Ord label) => Mean (Categorical label prob) where
    mean dist = fst $ argmax snd $ Map.toList $ pdfmap dist
    
-- | Extracts the element in the distribution with the highest probability
mostLikely :: Ord prob => Categorical label prob -> label
mostLikely dist = fst $ argmax snd $ Map.toList $ pdfmap dist

-- | Converts a distribution into a list of (sample,probability) pai
dist2list :: Categorical sampletype prob -> [(sampletype,prob)]
dist2list (Categorical pdfmap) = Map.toList pdfmap


instance 
    ( Ord label, Show label
    , Ord prob, Show prob, Fractional prob
    ) => PlottableDistribution (Categorical label prob) 
        where
    samplePoints (Categorical dist) = Map.keys dist
    plotType dist = Bar

-------------------------------------------------------------------------------
-- Morphisms

-- instance 
--     ( Ord label
--     , Num prob
--     ) => Morphism (Categorical label prob) FreeModParams (FreeMod prob label) 
--         where
--     Categorical pdf $> FreeModParams = FreeMod pdf
--     