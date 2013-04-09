{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The categorical distribution is used for discrete data.  It is also sometimes called the discrete distribution or the multinomial distribution.  For more, see the wikipedia entry: <https://en.wikipedia.org/wiki/CatContainer_distribution>
module HLearn.Models.Distributions.Multivariate.CatContainer
{-    ( 
    -- * Data types
    CatContainer (CatContainer)
    , CatContainerParams (..)
    
    -- * Helper functions
    , dist2list
    , mostLikely
    )-}
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
import HLearn.Models.Distributions.Multivariate.Unital

-------------------------------------------------------------------------------
-- data types

data CatParams baseparams  = CatParams
    { baseparams :: baseparams 
    }
    deriving (Read,Show,Eq,Ord)

instance (DefaultParams baseparams) => DefaultParams (CatParams baseparams ) where
    defparams = CatParams defparams

data CatContainer' label basedist prob = CatContainer'
    { params :: CatParams (Params basedist)
    , pdfmap :: !(Map.Map label basedist)
    , probmap :: !(Map.Map label prob)
    , catnumdp :: prob
    } 
--     deriving (Show,Read,Eq,Ord)

instance (Show basedist, Show (Params basedist), Show label, Show prob) => Show (CatContainer' label basedist prob) where
    show dist = "CatContainer' "
--               ++"{ "++"params="++show (params dist)
              ++"{ "++"pdfmap="++show (pdfmap dist)
              ++", catnumdp="++show (catnumdp dist)
              ++"}"

instance (NFData label, NFData prob, NFData basedist, NFData (Params basedist)) => 
    NFData (CatContainer' label basedist prob) 
        where
    rnf d = rnf $ pdfmap d

type CatContainer label basedist prob = RegSG2Group (CatContainer' label basedist prob)
--     deriving (Show,Semigroup,Monoid,RegularSemigroup)

-- newtype Cat label basedist prob = Cat (RegSG2Group (CatContainer' label basedist prob))
-- deriving instance (Semigroup (CatContainer label basedist prob)) => 
--     Semigroup (Cat label basedist prob)
-- deriving instance (Monoid (CatContainer label basedist prob)) => 
--     Monoid (Cat label basedist prob)
-- deriving instance (RegularSemigroup (CatContainer label basedist prob)) => 
--     RegularSemigroup (Cat label basedist prob)



-- type family CatContainer label basedist prob
-- type instance CatContainer label basedist prob = RegSG2Group (CatContainer' label basedist prob)

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, Num prob, Eq (Params basedist), Semigroup basedist) => Abelian (CatContainer' label basedist prob)
instance (Ord label, Num prob, Eq (Params basedist), Semigroup basedist) => Semigroup (CatContainer' label basedist prob) where
    d1 <> d2 = if params d1 /= params d2
        then error "CatContainer'.(<>): dart"
        else d1 
            { pdfmap = Map.unionWith (<>) (pdfmap d1) (pdfmap d2) 
            , probmap = Map.unionWith (+) (probmap d1) (probmap d2) 
            , catnumdp  = (catnumdp d1)+(catnumdp d2)
            } 

instance (Ord label, Num prob, Eq (Params basedist), RegularSemigroup basedist) => RegularSemigroup (CatContainer' label basedist prob) where
    inverse d1 = d1 
        { pdfmap = Map.map (inverse) (pdfmap d1)
        , probmap = Map.map negate (probmap d1)
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

instance ModelParams (CatContainer label basedist prob) 
        where
    type Params (CatContainer label basedist prob) = CatParams (Params basedist)
    getparams (SGJust model) = params model

instance 
    ( Ord label
    , Num prob
    , Eq (Params basedist)
    , HomTrainer basedist
    , Datapoint basedist ~ HList t0
    ) => HomTrainer (CatContainer label basedist prob) 
        where
    type Datapoint (CatContainer label basedist prob) = label `HCons` (Datapoint basedist)
    
    train1dp' params (dp:::basedp) = SGJust $ CatContainer' 
        { params = params
        , pdfmap = Map.singleton dp $ train1dp' (baseparams params) basedp
        , probmap = Map.singleton dp 1
        , catnumdp  = 1
        }

-------------------------------------------------------------------------------
-- Distribution

class NumDP model dp | model -> dp where
    numdp :: model -> dp

instance NumDP (Unital prob) prob where
    numdp (Unital prob) = prob
    
instance NumDP (CatContainer label basedist prob) prob where
    numdp (SGJust dist) = catnumdp dist

-- marginalizeRight :: (NumDP basedist prob) => CatContainer label basedist prob -> CatContainer label (Unital prob) prob
-- marginalizeRight (SGJust dist) = SGJust $ CatContainer'
--     { params = CatParams NoParams
--     , pdfmap = Map.map (Unital . numdp) (pdfmap dist) 
--     , probmap = error "probmap"
--     , catnumdp = catnumdp dist
--     }
-- -- marginalizeRight (SGJust dist) = Map.foldr mappend mempty (pdfmap dist)

instance 
    ( Ord label
    , Ord prob
    , Fractional prob
    , Show prob
    , PDF basedist basedp prob
    ) => PDF (CatContainer label basedist prob) (label,basedp) prob 
        where

    {-# INLINE pdf #-}
    pdf (SGJust dist) (label,basedp) = val*weight/(catnumdp dist)
        where
            weight = case Map.lookup label (probmap dist) of
                Nothing -> 0
                Just x  -> x
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
-- dist2list :: CatContainer label prob -> [(label,prob)]
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

ds= [ "test":::'g':::1:::1:::HNil
    , "test":::'f':::1:::2:::HNil
    , "toot":::'f':::2:::2:::HNil
    ]