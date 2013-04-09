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

-- | 
module HLearn.Models.Distributions.Multivariate
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

import HLearn.Models.Distributions.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.MultiNormal hiding (ds)
import HLearn.Models.Distributions.Moments

-------------------------------------------------------------------------------
-- data types

data Container dist sample basedist prob = Container    
    { dist :: dist prob
    , basedist :: basedist
    }
    
deriving instance (Show (dist prob), Show (Params basedist), Show basedist) => 
    Show (Container dist sample basedist prob)

-------------------------------------------------------------------------------
-- Algebra

instance 
    ( Semigroup (dist prob)
    , Semigroup basedist
    ) => Semigroup (Container dist sample basedist prob) 
        where
    c1<>c2 = Container
        { dist = dist c1 <> dist c2
        , basedist = basedist c1 <> basedist c2
        }
        
instance 
    ( Monoid (dist prob)
    , Monoid basedist
    , Semigroup (Container dist sample basedist prob) 
    ) => Monoid (Container dist sample basedist prob) 
        where
    mempty = Container mempty mempty
    c1 `mappend` c2 = c1<>c2

-------------------------------------------------------------------------------
-- Training

instance 
    ( ModelParams (dist prob)
    , ModelParams (basedist)
    , Params basedist ~ HList xs
    ) => ModelParams (Container dist sample basedist prob) 
        where
    type Params (Container dist sample basedist prob) = (Params (dist prob)) `HCons` (Params basedist)
    getparams c = (getparams $ dist c):::(getparams $ basedist c)
    
instance 
    ( HomTrainer (dist prob)
    , HomTrainer basedist
    , Params basedist ~ HList xs
    , Datapoint basedist ~ HList ys
    ) =>  HomTrainer (Container dist sample basedist prob) 
        where
    type Datapoint (Container dist sample basedist prob) = 
        (Datapoint (dist prob)) `HCons` (Datapoint basedist)
        
    train1dp' (distparams:::baseparams) (dp:::basedp) = Container
        { dist = train1dp' distparams dp
        , basedist = train1dp' baseparams basedp
        }

-------------------------------------------------------------------------------
-- Distribution
    
-------------------------------------------------------------------------------
-- test

ds2=[ "test":::'g':::1:::1:::HNil
    , "test":::'f':::1:::2:::HNil
    , "toot":::'f':::2:::2:::HNil
    ]
    
ds= [ "test":::'g':::1:::HNil
    , "test":::'f':::1:::HNil
    , "toot":::'f':::2:::HNil
    ]
    

    
-- testds = train ds :: MultiCategorical' '[String,Char] (Container Normal Double (Unital Double)) Double

testds = train ds2 :: MultivariateL
    '[ CatContainer' String
     , CatContainer' Char
     , Container Normal Double
     , Container Normal Double
     ]
     Double

data MultivariateL' (xs::[* -> * -> *]) prob = MultivariateL' (MultivariateL xs prob)

type family MultivariateL (xs::[* -> * -> *]) prob
-- type family MultivariateL (xs::a) prob
type instance MultivariateL '[] prob = Unital prob
type instance MultivariateL ((Container Normal prob) ': xs) prob = 
    Container Normal prob (MultivariateL xs prob) prob
type instance MultivariateL ((CatContainer' label) ': xs) prob = 
    CatContainer label (MultivariateL xs prob) prob

type family MultiCategorical (xs :: [*]) prob :: *
type instance MultiCategorical xs prob = MultiCategorical' xs Unital prob

type family MultiCategorical' (xs :: [*]) (basedist:: * -> *) prob :: *
type instance MultiCategorical' '[] basedist prob = basedist prob
type instance MultiCategorical' (x ': xs) basedist prob = 
    CatContainer x (MultiCategorical' xs basedist prob) prob
    
-- type family MultiCategorical'' (xs :: [*]) :: * -> * -> *
-- type instance MultiCategorical' '[] = Unital prob
-- type instance MultiCategorical' (x ': xs) = 
--     CatContainer x (MultiCategorical' xs basedist prob) prob