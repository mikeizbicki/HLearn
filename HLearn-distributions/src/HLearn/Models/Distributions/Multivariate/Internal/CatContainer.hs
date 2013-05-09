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
module HLearn.Models.Distributions.Multivariate.Internal.CatContainer
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
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization
import HLearn.Models.Distributions.Univariate.Categorical

-------------------------------------------------------------------------------
-- data types

data CatContainer label basedist prob = CatContainer
    { pdfmap :: !(Map.Map label basedist)
    , probmap :: !(Map.Map label prob)
    , catnumdp :: prob
    } 
    deriving (Show,Read,Eq,Ord)

-- instance (Show basedist, Show label, Show prob) => Show (CatContainer label basedist prob) where
--     show dist = "CatContainer "
-- --               ++"{ "++"params="++show (params dist)
--               ++"{ "++"pdfmap="++show (pdfmap dist)
--               ++", catnumdp="++show (catnumdp dist)
--               ++"}"

instance (NFData label, NFData prob, NFData basedist) => 
    NFData (CatContainer label basedist prob) 
        where
    rnf d = rnf $ pdfmap d

-- type CatContainer label basedist prob = RegSG2Group (CatContainer label basedist prob)

-------------------------------------------------------------------------------
-- Algebra

instance (Ord label, Num prob, Monoid basedist) => Abelian (CatContainer label basedist prob)
instance (Ord label, Num prob, Monoid basedist) => Monoid (CatContainer label basedist prob) where
    mempty = CatContainer mempty mempty 0
    d1 `mappend` d2 = CatContainer
        { pdfmap = Map.unionWith (<>) (pdfmap d1) (pdfmap d2) 
        , probmap = Map.unionWith (+) (probmap d1) (probmap d2) 
        , catnumdp  = (catnumdp d1)+(catnumdp d2)
        } 

instance (Ord label, Num prob, Group basedist) => Group (CatContainer label basedist prob) where
    inverse d1 = CatContainer
        { pdfmap = Map.map (inverse) (pdfmap d1)
        , probmap = Map.map negate (probmap d1)
        , catnumdp = -catnumdp d1
        }

instance (Num prob) => HasRing (CatContainer label basedist prob) where
    type Ring (CatContainer label basedist prob) = prob

instance 
    ( Ord label
    , Num prob
    , Module basedist
    , Ring basedist ~ Ring (CatContainer label basedist prob)
    ) => Module (CatContainer label basedist prob) 
        where
    r .* d = CatContainer
        { pdfmap = Map.map (r.*) (pdfmap d)
        , probmap = Map.map (r*) (probmap d)
        , catnumdp = r * catnumdp d
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
    ( Ord label
    , Num prob
    , HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) => HomTrainer (CatContainer label basedist prob) 
        where
    type Datapoint (CatContainer label basedist prob) = label `HCons` (Datapoint basedist)
    
    train1dp (dp:::basedp) = CatContainer 
        { pdfmap = Map.singleton dp $ train1dp basedp
        , probmap = Map.singleton dp 1
        , catnumdp  = 1
        }

instance (Num prob) => NumDP (CatContainer label basedist prob) where
    numdp dist = catnumdp dist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (CatContainer label basedist prob) where
    type Probability (CatContainer label basedist prob) = prob

instance 
    ( Ord prob, Fractional prob, Show prob, Probability basedist ~ prob
    , Ord label
    , PDF basedist
    , Datapoint basedist ~ HList ys
    , Show (Datapoint basedist)
    , Show label
    ) => PDF (CatContainer label basedist prob)
        where

    {-# INLINE pdf #-}
    pdf dist (label:::basedp) = val*weight/(catnumdp dist)
        where
            weight = case Map.lookup label (probmap dist) of
                Nothing -> 0
                Just x  -> x
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> trace ("Warning.CatContainer: label "++show label++" not found in training data: "++show (Map.keys $ pdfmap dist)) $ 0
                Just x  -> pdf x basedp

---------------------------------------
    
instance 
    ( NumDP basedist
    , Ring basedist ~ prob
    , Monoid basedist
    , HCons label (Datapoint basedist) ~ HList (label ': ts)
    , Ord label
    ) => Marginalize' (Nat1Box Zero) (CatContainer label basedist prob) 
        where
              
    type Margin' (Nat1Box Zero) (CatContainer label basedist prob) = (Categorical label prob) 
    getMargin' _ dist = Categorical $ probmap dist --Map.map numdp (pdfmap dist) 

    type MarginalizeOut' (Nat1Box Zero) (CatContainer label basedist prob) = Ignore' label basedist prob
    marginalizeOut' _ dist = Ignore' $ reduce $ Map.elems (pdfmap dist)  
        
    condition' _ dist dp = Ignore' $ 
        case Map.lookup dp (pdfmap dist) of
             Nothing -> error "CatContainer.condition: Nothing"
             Just basedist -> basedist
                                
{-    conditionAllButOne _ dist (dp:::dpL) = Ignore' $ 
        case Map.lookup dp (pdfmap dist) of
             Nothing -> error "CatContainer.condition: Nothing"
             Just basedist -> basedist-}
                                

instance 
    ( Marginalize' (Nat1Box n) basedist
    , Monoid basedist
    , PDF (Margin' (Nat1Box n) basedist)
    , prob ~ Probability (Margin' (Nat1Box n) basedist)
    , prob ~ Ring basedist
    , Module basedist
    , Ord label
    , Num prob
    ) => Marginalize' (Nat1Box (Succ n)) (CatContainer label basedist prob) 
        where
              
    type Margin' (Nat1Box (Succ n)) (CatContainer label basedist prob) = Margin' (Nat1Box n) basedist
    getMargin' _ dist = getMargin' (undefined :: Nat1Box n) $ reduce $ 
        zipWith (.*)
        (Map.elems $ probmap dist) 
        (Map.elems $ pdfmap dist) 
    
    type MarginalizeOut' (Nat1Box (Succ n)) (CatContainer label basedist prob) = 
        CatContainer label (MarginalizeOut' (Nat1Box n) basedist) prob
    marginalizeOut' _ dist = dist { pdfmap = fmap (marginalizeOut' (undefined :: Nat1Box n)) $ pdfmap dist }

    condition' _ dist dp = dist 
        { probmap = Map.unionWith (*) (probmap dist) (conditionmap)
        , pdfmap = fmap (flip (condition' (undefined :: Nat1Box n)) dp) $ pdfmap dist 
        }
        where
            conditionmap = fmap (\dist -> pdf (getMargin' (undefined :: Nat1Box n) dist) dp) $ pdfmap dist 
--     conditionAllButOne _ dist (dp:::dpL) = dist { pdfmap = fmap (flip (condition (undefined :: Nat1Box n)) dpL) $ pdfmap dist }
    
{-marginalizeRight :: 
    ( NumDP basedist prob
    ) => CatContainer label basedist prob -> CatContainer label (Unital prob) prob
marginalizeRight dist = CatContainer
    { pdfmap = Map.map (Unital . numdp) (pdfmap dist) 
    , probmap = error "probmap"
    , catnumdp = catnumdp dist
    }-}
-- marginalizeRight (SGJust dist) = Map.foldr mappend mempty (pdfmap dist)

    
-------------------------------------------------------------------------------
-- test

ds= [ "test":::'g':::"foo":::HNil
    , "test":::'f':::"fok":::HNil
    , "toot":::'f':::"foo":::HNil
    ]
    
test = train ds :: CatContainer String (CatContainer Char (CatContainer String (Unital Double) Double) Double) Double