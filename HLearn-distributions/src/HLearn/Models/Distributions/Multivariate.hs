{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}

-- | 

module HLearn.Models.Distributions.Multivariate
    ( 
    )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Categorical
import GHC.TypeLits

data Discrete
data Continuous
data Copula

data ParamsBox -- = forall (Model params dis) => params . ParamsBox params
data DistBox

data TypeList (n::Nat) a = TL a

-- tlEmpty :: TypeList 0 ()
tlEmpty = TL ()

tlCons :: a -> TypeList n b -> TypeList (n) (a,b)
tlCons a (TL b) = TL (a,b)

-- test = (CategoricalParams) `tlCons` tlEmpty

data MultivariateParams distparams {-copulaparams-} = MultivariateParams
    { distparams   :: distparams
--     , copulaparams :: copulaparams
    }
    deriving (Read,Show)
    
data Multivariate distparams {-copulaparams-} distL {-copula-} = Multivariate
    { params :: MultivariateParams distparams {-copulaparams-}
    , distL  :: distL
--     , copula :: copula
    }
    deriving (Read,Show)
    
instance Model (MultivariateParams distparams) (Multivariate distparams distL) where
    getparams = params
    
instance 
    ( DefaultModel params dist
    , DefaultModel paramsT distT
    ) => DefaultModel (MultivariateParams (params,paramsT)) (Multivariate (params,paramsT) (dist,distT)) 
        where
    defparams = MultivariateParams (defparams,defparams)
    
instance Semigroup (Multivariate distparams distL) where
    (<>) = undefined
instance Monoid (Multivariate distparams distL) where
    mempty=undefined
    mappend=(<>)
    
instance HomTrainer
    (MultivariateParams () ) 
    ()
    (Multivariate () ()) 
        where
    train1dp' tlEmpty dp = Multivariate (MultivariateParams ()) ()

instance Model () () where
    getparams () = ()

instance DefaultModel () () where
    defparams = ()
    
instance 
    ( DefaultModel params1 model1
    , DefaultModel params2 model2
    , Model (params1,params2) (model1,model2)
    ) => DefaultModel (params1,params2) (model1,model2) where
    defparams = (defparams,defparams)

instance 
    ( Model params1 model1
    , Model params2 model2
    ) => Model (params1,params2) (model1,model2) where
    getparams (model1,model2) = (getparams model1,getparams model2)

instance HomTrainer () () ()
        where
    train1dp' () () = ()

instance 
    ( HomTrainer params1 dp1 model1
    , HomTrainer params2 dp2 model2
    ) => HomTrainer (params1,params2) (dp1,dp2) (model1,model2)
        where
    train1dp' (params1,params2) (dp1,dp2) = (train1dp' params1 dp1, train1dp' params2 dp2)

instance 
    ( HomTrainer params dp dist
    , HomTrainer paramsT dpT distT
    ) => HomTrainer (MultivariateParams (params,paramsT)) (dp,dpT) (Multivariate (params,paramsT) (dist,distT)) 
        where
    train1dp' (MultivariateParams (params,paramsT)) (dp,dpT) = 
        Multivariate (MultivariateParams (params,paramsT)) (train1dp' params dp,train1dp' paramsT dpT)

-- test = train1dp' (MultivariateParams (CategoricalParams,())) (4::Int,()) 
test = train1dp ("food",(6::Int,(4::Int,())))
    :: Multivariate (CategoricalParams,(CategoricalParams,(CategoricalParams,()))) 
        (Categorical String Double, (Categorical Int Double,(Categorical Int Double,())))

-- instance HomTrainer
--     (MultivariateParams (TypeList (n) (params,ptl)) ) 
--     (TypeList (n) (dp,dptl)) 
--     (Multivariate (TypeList (n) (params,ptl)) (TypeList (n) (dist,disttl)) ) 
--         where
--     train1dp' (MultivariateParams (TL (params,ptl))) dp = 
--         Multivariate (MultivariateParams (TL (params,ptl))) $ TL (train1dp' params dp,ptl)

        
-- tlLength :: (Int,a) -> Int
-- tlLength (n,_) = n
-- 
-- tlCons :: item -> (Int,a) -> (Int,(item,a))
-- tlCons item (n,a) = (n+1,(item,a))

-- replicateTuple :: a -> (n::Nat) -> n+1
-- replicateTuple a n = undefined