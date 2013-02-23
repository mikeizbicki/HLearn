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
    ( MultivariateParams (..)
    , Multivariate
    , (:::) (..)
    , (:::.) (..)
    )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
import GHC.TypeLits

-------------------------------------------------------------------------------
-- Uniform

data UniformParams prob = UniformParams

data Uniform prob = Uniform

instance (Num prob) => Distribution (Uniform prob) dp prob where
    pdf _ _ = 1

-- data Index (n::Nat) -- = Index n

-- class Indexable t (i::Nat) v where
--     get :: t -> (Index i) -> v
-- 
-- instance Indexable (a:::b) 0 a where
--     get (a:::b) _ = a

data Nat1 = Zero | Succ Nat1
type family FromNat1 (n :: Nat1) :: Nat
type instance FromNat1 Zero     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n

-- type family (:!!) (list::[a]) (n::Nat1) :: a
-- type instance (:!!) (x ': xs) Zero = x
-- type instance (:!!) (x ': xs) (Succ n) = xs :!! n

type family (:!!) t (n::Nat1) :: a
type instance (:!!) (a:::b) Zero = b
type instance (:!!) (a:::b) (Succ n) = a :!! n

-- data Index (n::Nat1)
-- 
-- class Fetchable tuple (n::Nat1) ret where
--     fetch :: tuple -> Index n -> ret -- t :!! n
--     
-- instance Fetchable a Zero a where
--     fetch a _ = a
--     
-- -- instance Fetchable (a:::b) 0 a where
-- --     fetch (a:::b) _ = a
--     
-- -- instance (Fetchable b 0 c) => Fetchable (a:::b) 0 c where
-- --     fetch (a:::b) n = fetch b n
-- 
-- data Selector (n::Nat)
-- data Test where
--     Test :: Selector (FromNat1 n) -> Nat -> Test
-- 
-- -- poop :: (Distribution dist b prob) => (c,dist) -> (a:::b:::c) -> prob
-- -- poop (_,dist) (a:::b:::c) = pdf dist b
-- --     
-- -- data Nat1 = Zero | Succ Nat1
-- -- 
-- type family Get (n :: Nat1) x
-- -- type instance Get Zero     (x)      = x
-- type instance Get Zero     (x:::xs) = x
-- type instance Get (Succ n) (x:::xs) = Get n xs
-- 
-- data HList :: [*] -> * where
--   HNil  :: HList '[]
--   HCons :: a -> HList t -> HList (a ': t)
--     
-- -- class TupLen t where
-- --     t :: t -> (n::Nat)
--     
-- -- instance TypeIndex a where
-- --     f a = 0
-- --     typeindex :: tuple -> Int -> ret
--     
-- -- instance TypeIndex (a:::b) a where
-- --     typeindex (a:::b) 0 = a
--     
-- 
--     
-- -- getmargin :: (n::Nat) -> Int
-- -- getmargin = undefined

-- type family Replicate (n::Nat1) prob
-- type instance Replicate Zero prob = ()
-- type instance Replicate (FromNat1 n) prob = prob ::: (Replicate  prob)

{-type family FromNat1 (n :: Nat1) :: Nat
type instance FromNat1 Zero     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n-}
-------------------------------------------------------------------------------
-- Crazy Tuple

data a ::: b = a ::: b
    deriving (Read,Show,Eq,Ord)

data (:::.) a b c = a c :::. b c
    deriving (Read,Show,Eq,Ord)

instance (Abelian (a c), Abelian (b c)) => Abelian ((a:::.b) c)
instance (Semigroup (a c), Semigroup (b c)) => Semigroup ((a:::.b) c) where
    (a1 :::. b1) <> (a2 :::. b2) = (a1<>a2):::.(b1<>b2)

instance (Monoid (a c), Monoid (b c)) => Monoid ((a:::.b) c) where
    mempty = mempty :::. mempty
    (a1 :::. b1) `mappend` (a2 :::. b2) = (a1 `mappend` a2):::.(b1 `mappend` b2)
    
instance (RegularSemigroup (a c), RegularSemigroup (b c)) => RegularSemigroup ((a:::.b) c) where
    inverse (a :::. b) = (inverse a):::.(inverse b)

instance (LeftModule r (a c), LeftModule r (b c)) => LeftModule r ((a:::.b) c) where
instance (LeftOperator r (a c), LeftOperator r (b c)) => LeftOperator r ((a:::.b) c)where
    r.*(a:::.b) = (r.*a):::.(r.*b)
    
instance (RightModule r (a c), RightModule r (b c)) => RightModule r ((a:::.b) c) where
instance (RightOperator r (a c), RightOperator r (b c)) => RightOperator r ((a:::.b) c) where
    (a:::.b)*.r = (a*.r):::.(b*.r)

instance 
    ( Model (params1 prob) (model1 prob)
    , Model (params2 prob) (model2 prob)
    ) => Model ((params1:::.params2) prob) ((model1:::.model2) prob) where
    getparams (model1 :::. model2) = (getparams model1 :::. getparams model2)

instance 
    ( DefaultModel (params1 prob) (model1 prob)
    , DefaultModel (params2 prob) (model2 prob)
    , Model ((params1:::.params2) prob) ((model1:::.model2) prob)
    ) => DefaultModel ((params1:::.params2) prob) ((model1:::.model2) prob) where
    defparams = (defparams :::. defparams)

instance 
    ( HomTrainer (params1 prob) dp1 (model1 prob)
    , HomTrainer (params2 prob) dp2 (model2 prob)
    ) => HomTrainer ((params1:::.params2) prob) (dp1 ::: dp2) ((model1:::.model2) prob)
        where
    train1dp' (params1:::.params2) (dp1 ::: dp2) = (train1dp' params1 dp1:::.train1dp' params2 dp2)
--     train1dp' (params1 :::. params2) (dp1 :::. dp2) = (train1dp' params1 dp1 :::. train1dp' params2 dp2)

-- instance 
--     ( HomTrainer params ((dp1,dp2),dp3) model
--     ) => HomTrainer params (dp1,dp2,dp3) model
--         where
--     train1dp' params (dp1,dp2,dp3) = train1dp' params ((dp1,dp2),dp3)

instance 
    ( Num prob
    , Distribution (dist prob) dp prob
    , Distribution (distT prob) dpT prob
    ) => Distribution ((dist:::.distT) prob) (dp:::dpT) prob where
    pdf (dist:::.distT) (dp:::dpT) = (pdf dist dp)*(pdf distT dpT)

-------------------------------------------------------------------------------
-- Multivariate

data MultivariateParams marginparams = MultivariateParams
    { distparams   :: marginparams
    }
    deriving (Read,Show,Eq,Ord)
    
data Multivariate' margins prob = Multivariate' 
    { margins  :: margins prob
    }
    deriving (Read,Show,Eq,Ord)

type Multivariate margins prob = RegSG2Group (Multivariate' margins prob)

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian (margins prob), Model marginparams (margins prob)) => Abelian (Multivariate' margins prob)
instance (Semigroup (margins prob), Model marginparams (margins prob)) => Semigroup (Multivariate' margins prob) where
    mv1 <> mv2 = if (getparams $ margins mv1)/=(getparams $ margins mv2)
        then error "Multivariate.(<>): Adding distributions with different params"
        else mv1 { margins = (margins mv1) <> (margins mv2) }

-- instance (RegularSemigroup margins, Model marginparams margins) => RegularSemigroup (Multivariate' margins) where
--     inverse mv = mv { margins = inverse $ margins mv }
-- 
-- instance (Model marginparams margins, LeftModule ring margins) => LeftModule ring (Multivariate margins)
-- instance (LeftOperator ring margins) => LeftOperator ring (Multivariate' margins) where
--     r .* m = m { margins = r .* (margins m) }
--     
-- instance (Model marginparams margins, RightModule ring margins) => RightModule ring (Multivariate margins)
-- instance (RightOperator ring margins) => RightOperator ring (Multivariate' margins) where
--     m *. r = m { margins = (margins m) *. r }

-------------------------------------------------------------------------------
-- Training

instance (RegularSemigroup (margins prob), Model (marginparams prob) (margins prob)) => 
    Model (MultivariateParams (marginparams prob)) (Multivariate margins prob) 
        where
    getparams (SGJust mv) = MultivariateParams $ getparams (margins mv)
    
instance 
    ( DefaultModel (marginparams prob) (margins prob)
    , RegularSemigroup (margins prob)
    ) => DefaultModel (MultivariateParams (marginparams prob)) (Multivariate margins prob) 
        where
    defparams = MultivariateParams defparams

instance 
    ( HomTrainer (marginparams prob) dp (margins prob)
    , RegularSemigroup (margins prob)
    ) => HomTrainer (MultivariateParams (marginparams prob)) dp (Multivariate margins prob) 
        where
    train1dp' (MultivariateParams marginparams) dp = 
        SGJust $ Multivariate' (train1dp' marginparams dp)

-------------------------------------------------------------------------------
-- Distribution

instance 
    ( RegularSemigroup (dist prob)
    , Distribution (dist prob) dp prob
    ) => Distribution (Multivariate dist prob) dp prob where
    pdf (SGJust dist) = pdf (margins dist) 

-------------------------------------------------------------------------------
-- Testing

{-test = train1dp ("test" ::: 6 ::: 4)
    :: Multivariate 
        (Categorical String:::.Categorical Int:::.Categorical Int) Float

test2 = train 
    [("food" ::: "dog")
    ,("food" ::: "cat")
    ,("food" ::: "water")
    ,("drink" ::: "water")
    ,("drink" ::: "piss")
    ]
    :: Multivariate (Categorical String :::. Categorical String) Double-}
        