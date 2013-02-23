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

module HLearn.Algebra.Tuple
    where

import qualified Data.Vector as V
import GHC.TypeLits

data Country = USA | Canada

data Human = Human
    { age :: Int
    , citizenship :: Country
    , money 
    }

-- dp = "test":::123:::22:::USA

-------------------------------------------------------------------------------
-- data types

newtype Tuple (xs::[*]) = Tuple 
    { getvec :: V.Vector Int
    }

data a:::b = a:::b

-- class MkTup a where
--     mktup :: a -> Tuple
--     
-- test = mktup $ 1:::2:::3

data DP discrete real = DP (discrete) (real)

data Index (n::Nat)

index :: Tuple xs -> Index (FromNat1 i) -> xs :! i
index = undefined

-- type family Tuple (xs::[*]) :: *
-- type instance Tuple xs = Tuple' (Length xs) xs
-- newtype Tuple' (len::Nat) (xs::[*]) = Tuple 
--     { getvec :: V.Vector Int
--     }


type family Length (xs::[*]) :: Nat
type instance Length '[] = 0
type instance Length (a ': xs) = 1 + (Length xs)

type family (:!) (xs::[*]) (i::Nat1) :: *
type instance (:!) '[] i = ()
type instance (:!) (x ': xs) Zero = x

data Nat1 = Zero | Succ Nat1
type family FromNat1 (n :: Nat1) :: Nat
type instance FromNat1 Zero     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n


-- type family Index tup (i::Nat) :: *
-- type instance Index (Tuple 0 '[]) i = ()
-- type instance Index (Tuple 0 '[]) i = ()
   
-- data family (:!) tup
-- type instance (:!) (Tuple 0 []) i = ()