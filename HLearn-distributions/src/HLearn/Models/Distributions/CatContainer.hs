{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | The categorical distribution is used for discrete data.  It is also sometimes called the discrete distribution or the multinomial distribution.  For more, see the wikipedia entry: <https://en.wikipedia.org/wiki/CatContainer_distribution>
module HLearn.Models.Distributions.CatContainer
    ( 
{-    -- * Data types
    CatContainer (CatContainer)
    , CatContainerParams (..)
    
    -- * Helper functions
    , dist2list
    , mostLikely-}
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
import HLearn.Models.Distributions.Unital

-------------------------------------------------------------------------------
-- data types

data CatParams sampletype prob baseparams = CatParams
    { baseparams :: baseparams 
    }
    deriving (Read,Show,Eq,Ord)

data CatContainer' sampletype prob baseparams basedist = CatContainer'
    { params :: CatParams sampletype prob baseparams
    , pdfmap :: !(Map.Map sampletype basedist)
    , catnumdp :: prob
    } 
    deriving (Show,Read,Eq,Ord)

instance (NFData sampletype, NFData prob, NFData baseparams, NFData basedist) => NFData (CatContainer' sampletype prob baseparams basedist) where
    rnf d = rnf $ pdfmap d

type CatContainer sampletype prob baseparams basedist = 
    RegSG2Group (CatContainer' sampletype prob baseparams basedist)

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, Num prob, Eq baseparams, Semigroup basedist) => Abelian (CatContainer' label prob baseparams basedist)
instance (Ord label, Num prob, Eq baseparams, Semigroup basedist) => Semigroup (CatContainer' label prob baseparams basedist) where
    d1 <> d2 = if params d1 /= params d2
        then error "CatContainer'.(<>): dart"
        else d1 
            { pdfmap = Map.unionWith (<>) (pdfmap d1) (pdfmap d2) 
            , catnumdp  = (catnumdp d1)+(catnumdp d2)
            } 

instance (Ord label, Num prob, Eq baseparams, RegularSemigroup basedist) => RegularSemigroup (CatContainer' label prob baseparams basedist) where
    inverse d1 = d1 
        { pdfmap = Map.map (inverse) (pdfmap d1)
        , catnumdp = -catnumdp d1
        }

-- -- instance (Ord label, Num prob) => LeftModule prob (CatContainer label prob)
-- instance (Ord label, Num prob) => LeftOperator prob (CatContainer label prob) where
--     p .* (CatContainer pdf) = CatContainer $ Map.map (*p) pdf
-- 
-- -- instance (Ord label, Num prob) => RightModule prob (CatContainer label prob)
-- instance (Ord label, Num prob) => RightOperator prob (CatContainer label prob) where
--     (*.) = flip (.*)

-------------------------------------------------------------------------------
-- Training

instance 
    ( Eq baseparams
    ) => Model (CatParams label prob baseparams) (CatContainer label prob baseparams basedist) 
        where
    getparams (SGJust model) = params model

instance 
    ( Eq baseparams
    , DefaultModel baseparams basedist
    ) => DefaultModel (CatParams label prob baseparams) (CatContainer label prob baseparams basedist) 
        where
    defparams = CatParams defparams
    
instance 
    ( Ord label
    , Num prob
    , HomTrainer baseparams basedp basedist
    ) => HomTrainer (CatParams label prob baseparams) (label,basedp) (CatContainer label prob baseparams basedist) 
        where
    train1dp' params (dp,basedp) = SGJust $ CatContainer' 
        { params = params
        , pdfmap = Map.singleton dp $ train1dp' (baseparams params) basedp
        , catnumdp  = 1
        }

-------------------------------------------------------------------------------
-- Distribution

class NumDP model dp | model -> dp where
    numdp :: model -> dp

instance NumDP (Unital prob) prob where
    numdp (Unital prob) = prob
    
instance NumDP (CatContainer label prob baseparams basedist) prob where
    numdp (SGJust dist) = catnumdp dist

-- class Marginalizable dist where
--     marginalizeRight :: 

marginalizeRight :: (NumDP basedist prob) => CatContainer label prob baseparams basedist -> CatContainer label prob (NoParams (Unital prob)) (Unital prob)
marginalizeRight (SGJust dist) = SGJust $ CatContainer'
    { params = CatParams NoParams
    , pdfmap = Map.map (Unital . numdp) (pdfmap dist) 
    , catnumdp = catnumdp dist
    }
-- marginalizeRight (SGJust dist) = Map.foldr mappend mempty (pdfmap dist)

instance 
    ( Ord label
    , Ord prob
    , Fractional prob
    , Distribution basedist basedp prob
    ) => Distribution (CatContainer label prob baseparams basedist) (label,basedp) prob 
        where

    {-# INLINE pdf #-}
    pdf (SGJust dist) (label,basedp) = (val/(catnumdp dist))
        where
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> 0
                Just x  -> pdf x basedp

---------------------------------------

-- instance (Ord label, Ord prob, Fractional prob) => CDF (CatContainer label prob) label prob where
-- 
--     {-# INLINE cdf #-}
--     cdf dist label = (Map.foldl' (+) 0 $ Map.filterWithKey (\k a -> k<=label) $ pdfmap dist) 
--                    / (Map.foldl' (+) 0 $ pdfmap dist)
--                    
--     {-# INLINE cdfInverse #-}
--     cdfInverse dist prob = go cdfL
--         where
--             cdfL = sortBy (\(k1,p1) (k2,p2) -> compare p2 p1) $ map (\k -> (k,pdf dist k)) $ Map.keys $ pdfmap dist
--             go (x:[]) = fst $ last cdfL
--             go (x:xs) = if prob < snd x -- && prob > (snd $ head xs)
--                 then fst x
--                 else go xs
-- --     cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist
-- 
-- --     {-# INLINE mean #-}
-- --     mean dist = fst $ argmax snd $ Map.toList $ pdfmap dist
-- -- 
-- --     {-# INLINE drawSample #-}
-- --     drawSample dist = do
-- --         x <- getRandomR (0,1)
-- --         return $ cdfInverse dist (x::prob)
-- 
-- 
-- -- | Extracts the element in the distribution with the highest probability
-- mostLikely :: Ord prob => CatContainer label prob -> label
-- mostLikely dist = fst $ argmax snd $ Map.toList $ pdfmap dist
-- 
-- -- | Converts a distribution into a list of (sample,probability) pai
-- dist2list :: CatContainer sampletype prob -> [(sampletype,prob)]
-- dist2list (CatContainer pdfmap) = Map.toList pdfmap


-------------------------------------------------------------------------------
-- Morphisms

-- instance 
--     ( Ord label
--     , Num prob
--     ) => Morphism (CatContainer label prob) FreeModParams (FreeMod prob label) 
--         where
--     CatContainer pdf $> FreeModParams = FreeMod pdf

    
    
    
-------------------------------------------------------------------------------
-- test

ds= [ (1,(1,()))
    , (1,(2,()))
    , (2,(2,()))
    ] :: [(Int,(Int,()))]
    
type family Cat label prob basedist :: *
-- type instance Cat (label,()) prob = CatContainer label prob (NoParams (Unital prob)) (Unital prob)
type instance Cat label prob basedist = CatContainer label prob (Params basedist) basedist

type family Categorical labelL prob :: *
type instance Categorical () prob = Unital prob
type instance Categorical (label,basedp) prob = CatContainer label prob (Params (Categorical basedp prob)) (Categorical basedp prob)

type family Params model :: *
type instance Params (CatContainer label prob baseparams basedist) = CatParams label prob baseparams
type instance Params (Unital prob) = NoParams (Unital prob)

test = train ds :: CatContainer Int Double (CatParams Int Double (NoParams (Unital Double))) (CatContainer Int Double (NoParams (Unital Double)) (Unital Double))
