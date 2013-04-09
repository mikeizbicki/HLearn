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
module HLearn.Models.Distributions.Multivariate.Multivariate
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
import HLearn.Models.Distributions.Multivariate.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.Multivariate.MultiNormal hiding (ds)
import HLearn.Models.Distributions.Univariate.Moments

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
    
newtype MultiCat (sampleL :: [*]) (basedist :: *) prob = MultiCat
    { catL :: MultiCat' sampleL basedist prob
    }

type family MultiCat' (xs :: [*]) (basedist:: *) prob :: *
type instance MultiCat' '[] basedist prob = basedist
type instance MultiCat' (x ': xs) basedist prob = 
    CatContainer x (MultiCat' xs basedist prob) prob

type family Multi' (dist :: * -> * -> * -> *) (sampleL :: [*]) (basedist :: *) prob
type instance Multi' dist '[] basedist prob = basedist
type instance Multi' dist (x ': xs) basedist prob =
    dist x (Multi' dist xs basedist prob) prob

newtype MultiContainer (dist :: * -> *) (sampleL :: [*]) (basedist :: *) prob = Multi
    { multiL :: MultiContainer' dist sampleL basedist prob
    }

type family MultiContainer' (dist :: * -> *) (sampleL :: [*]) (basedist :: *) prob
type instance MultiContainer' dist '[] basedist prob = basedist
type instance MultiContainer' dist (x ': xs) basedist prob =
    Container dist x (MultiContainer' dist xs basedist prob) prob

testds = {-train ds2-} undefined :: Multivariate
    '[ MultiCat '[String,Char]
     , MultiContainer Normal '[Double, Double]
     ]
     Double

data Multivariate (xs::[* -> * -> *]) prob = Multivariate
    { multidist :: MultivariateL xs prob
    }

type family MultivariateL (xs::[* -> * -> *]) prob
type instance MultivariateL '[] prob = Unital prob
type instance MultivariateL ((Container Normal prob) ': xs) prob = 
    Container Normal prob (MultivariateL xs prob) prob
type instance MultivariateL ((CatContainer' label) ': xs) prob = 
    CatContainer label (MultivariateL xs prob) prob
