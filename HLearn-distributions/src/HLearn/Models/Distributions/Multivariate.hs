{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE DatatypeContexts #-}

-- | 

module HLearn.Models.Distributions.Multivariate
{-    ( MultivariateParams (..)
    , Multivariate
    , (:::) (..)
    )-}
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
import GHC.TypeLits

-- data Discrete
-- data Continuous
-- data Copula

-- data ParamsBox = forall (Model params dist) => params . ParamsBox params
-- data DistBox

-------------------------------------------------------------------------------
-- Crazy Tuple

-- data T a b c = (a c) ::: (b c)
--     deriving (Read,Show,Eq,Ord)

data a ::: b = a ::: b
    deriving (Read,Show,Eq,Ord)

instance (Semigroup a, Semigroup b) => Semigroup (a ::: b) where
    (a1 ::: b1) <> (a2 ::: b2) = (a1<>a2):::(b1<>b2)
    
instance (Monoid a, Monoid b) => Monoid (a ::: b) where
    mempty = mempty ::: mempty
    (a1 ::: b1) `mappend` (a2 ::: b2) = (a1 `mappend` a2):::(b1 `mappend` b2)
    
instance (RegularSemigroup a, RegularSemigroup b) => RegularSemigroup (a ::: b) where
    inverse (a ::: b) = (inverse a):::(inverse b)

-------------------------------------------------------------------------------
-- Multivariate

data MultivariateParams distparams {-copulaparams-} = MultivariateParams
    { distparams   :: distparams
--     , copulaparams :: copulaparams
    }
    deriving (Read,Show,Eq,Ord)
    
data Multivariate' distparams {-copulaparams-} distL {-copula-} = Multivariate'
    { params :: MultivariateParams distparams {-copulaparams-}
    , distL  :: distL
--     , copula :: copula
    }
    deriving (Read,Show,Eq,Ord)

-- data Multivariate' distparams {-copulaparams-} distL {-copula-} = Multivariate'
--     { params :: MultivariateParams distparams {-copulaparams-}
--     , distL  :: distL
-- --     , copula :: copula
--     }
--     deriving (Read,Show,Eq,Ord)

data ParamsMask dist = forall params dist . (Model params dist) => ParamsMask dist

-- type Multivariate distL = forall distparams distL . (Model distparams distL) => RegSG2Group (Multivariate' distparams distL)
type Multivariate distparams distL = RegSG2Group (Multivariate' distparams distL)
--  Multivariate distL = forall (Model distparams distL) => RegSG2Group (Multivariate' distparams distL)
    
-- data Booger distL = forall distparams distL . (Model distparams distL) => Booger (Multivariate distparams distL)
    
-------------------------------------------------------------------------------
-- Algebra

instance (Semigroup distL, Eq distparams) => Semigroup (Multivariate' distparams distL) where
    mv1 <> mv2 = if (params mv1)/=(params mv2)
        then error "Multivariate.(<>): Adding distributions with different params"
        else mv1 { distL = (distL mv1) <> (distL mv2) }

instance (RegularSemigroup distL, Eq distparams) => RegularSemigroup (Multivariate' distparams distL) where
    inverse mv = mv { distL = inverse $ distL mv }

-------------------------------------------------------------------------------
-- Training

instance 
    ( Model params1 model1
    , Model params2 model2
    ) => Model (params1 ::: params2) (model1 ::: model2) where
    getparams (model1 ::: model2) = (getparams model1 ::: getparams model2)

instance 
    ( DefaultModel params1 model1
    , DefaultModel params2 model2
    , Model (params1 ::: params2) (model1 ::: model2)
    ) => DefaultModel (params1 ::: params2) (model1 ::: model2) where
    defparams = (defparams ::: defparams)

instance (RegularSemigroup distL, Eq distparams) => Model (MultivariateParams distparams) (Multivariate distparams distL) where
    getparams (SGJust mv) = params mv
    
instance 
    ( DefaultModel params dist
    , DefaultModel paramsT distT
    , RegularSemigroup dist
    , RegularSemigroup distT
    , Eq params
    , Eq paramsT
    ) => DefaultModel (MultivariateParams (params ::: paramsT)) (Multivariate (params ::: paramsT) (dist ::: distT)) 
        where
    defparams = MultivariateParams (defparams ::: defparams)

instance 
    ( HomTrainer params1 dp1 model1
    , HomTrainer params2 dp2 model2
    ) => HomTrainer (params1 ::: params2) (dp1 ::: dp2) (model1 ::: model2)
        where
    train1dp' (params1 ::: params2) (dp1 ::: dp2) = (train1dp' params1 dp1 ::: train1dp' params2 dp2)

instance 
    ( HomTrainer params dp dist
    , HomTrainer paramsT dpT distT
    , RegularSemigroup dist
    , RegularSemigroup distT
    , Eq paramsT
    , Eq params
    ) => HomTrainer (MultivariateParams (params ::: paramsT)) (dp ::: dpT) (Multivariate (params ::: paramsT) (dist ::: distT)) 
        where
    train1dp' (MultivariateParams (params ::: paramsT)) (dp ::: dpT) = 
        SGJust $ Multivariate' (MultivariateParams (params ::: paramsT)) (train1dp' params dp ::: train1dp' paramsT dpT)

-------------------------------------------------------------------------------
-- Distribution

instance 
    ( Eq params
    , RegularSemigroup dist
    , Distribution dist dp prob
    ) => Distribution (Multivariate params dist) dp prob where
    pdf (SGJust dist) = pdf (distL dist) 

instance 
    ( Num prob
    , Distribution dist dp prob
    , Distribution distT dpT prob
    ) => Distribution (dist:::distT) (dp:::dpT) prob where
    pdf (dist:::distT) (dp:::dpT) = (pdf dist dp)*(pdf distT dpT)

-- instance 
--     ( Eq params
--     , Eq paramsT
--     , RegularSemigroup dist
--     , RegularSemigroup distT
--     , Distribution dist dp prob
--     ) => Distribution (Multivariate (params ::: paramsT) (dist:::distT)) (dp:::dpT) prob where
--     pdf (SGJust (Multivariate' params (dist:::distT))) (dp ::: dpT) = (pdf dist dp)*(pdf () dpT)

-------------------------------------------------------------------------------
-- Testing

test = train1dp ("food":::6:::4)
    :: Multivariate (CategoricalParams ::: CategoricalParams ::: CategoricalParams)
        (Categorical String Double ::: Categorical Int Double ::: Categorical Int Double)

test2 = train [("food" ::: "dog")
              ,("food" ::: "cat")
              ,("drink" ::: "water")
              ,("drink" ::: "piss")
              ]
              :: Multivariate (CategoricalParams ::: CategoricalParams) (Categorical String Double ::: Categorical String Double)
                  