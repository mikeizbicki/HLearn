{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE PolyKinds #-}
-- | 

module HLearn.Models.Distributions.Multivariate
--     ( Multivariate
--     )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
import GHC.TypeLits

-------------------------------------------------------------------------------
-- data types

data MultivariateParams distparams copulaparams = MultivariateParams distparams copulaparams
    deriving (Read,Show,Eq,Ord)

data Multivariate' distparams copulaparams distL copula prob = Multivariate' 
    { params :: MultivariateParams distparams copulaparams
    , distL :: (HList (Distribute distL prob)) 
    , copula :: copula prob 
    }
--     deriving (Show,Eq,Ord)

type Multivariate distparams copulaparams distL copula prob = 
    RegSG2Group (Multivariate' distparams copulaparams distL copula prob)

instance 
    ( Semigroup (HList (Distribute distL prob))
    , Semigroup (copula prob)
    , Eq distparams
    , Eq copulaparams
    ) => Semigroup (Multivariate' distparams copulaparams distL copula prob) 
        where
    (Multivariate' params1 distL1 copula1)<>(Multivariate' params2 distL2 copula2) = 
        if (params1/=params2)
            then error "Multivariate.(<>): params not equal"
            else Multivariate' 
                { params = params1
                , distL = (distL1<>distL2) 
                , copula = (copula1<>copula2)
                }

instance
    ( RegularSemigroup (HList (Distribute distL prob))
    , RegularSemigroup (copula prob)
    , Eq distparams
    , Eq copulaparams
    ) => RegularSemigroup (Multivariate' distparams copulaparams distL copula prob)
        where
    inverse mv = mv
        { distL = inverse $ distL mv
        , copula = inverse $ copula mv
        }

-- instance 
--     ( Monoid (HList distL)
--     , Monoid copula
--     ) => Monoid (Multivariate' distL copula) 
--         where
--     mempty = Multivariate' mempty mempty
--     mappend (Multivariate' distL1 copula1) (Multivariate' distL2 copula2) = Multivariate' (distL1`mappend`distL2) (copula1`mappend`copula2)

-------------------------------------------------------------------------------
-- Uniform

data UniformParams prob = UniformParams

data Uniform prob = Uniform

instance (Num prob) => Distribution (Uniform prob) dp prob where
    pdf _ _ = 1

instance Semigroup (Uniform prob) where
    Uniform <> Uniform = Uniform
    
instance Monoid (Uniform prob) where
    mempty = Uniform
    mappend _ _ = Uniform