{-# LANGUAGE DataKinds,TypeOperators #-}
module HLearn.Algebra.Types.Frac
    where

import Data.Proxy
import GHC.TypeLits


data Frac = (/) Nat Nat

class KnownFrac (n :: Frac) where
    fracSing :: SFrac n 

instance (KnownNat a, KnownNat b) => KnownFrac (a/b) where
    fracSing = SFrac (natVal (Proxy::Proxy a)) (natVal (Proxy::Proxy b))

fracVal :: forall n proxy. (KnownFrac n) => proxy n -> Rational
fracVal _ = case fracSing :: SFrac n of
    SFrac a b -> fromIntegral a / fromIntegral b 

data SFrac (n::Frac) = SFrac Integer Integer 

---------------------------------------
-- GHC 7.6
-- 
-- data Frac = Frac Nat Nat 
-- 
-- data instance Sing (n::Frac) = SFrac Integer Integer
-- 
-- instance (SingI a, SingI b) => SingI ('Frac a b) where
--     sing = SFrac (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))
-- 
-- instance Fractional r => SingE (Kind :: Frac) r where
--     fromSing (SFrac a b) = fromIntegral a/fromIntegral b
-- 
-- type (/) a b = 'Frac a b
-- 
