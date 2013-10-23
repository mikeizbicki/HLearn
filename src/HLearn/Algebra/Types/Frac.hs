{-# LANGUAGE DataKinds,TypeOperators #-}
module HLearn.Algebra.Types.Frac
    where

import GHC.TypeLits

---------------------------------------

data Frac = (/) Nat Nat 

data instance Sing (n::Frac) = SFloat Integer Integer

instance (SingI a, SingI b) => SingI ((/) a b) where
    sing = SFloat (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))

instance Fractional r => SingE (Kind :: Frac) r where
    fromSing (SFloat a b) = fromIntegral a/fromIntegral b

---------------------------------------

-- type family (#) (a::Nat) (b::Nat) :: Frac
-- type instance (#) a b = a:.b
-- 
-- data Frac = (:.) Nat Nat
-- 
-- data instance Sing (n::Frac) = SFrac Integer Integer
-- 
-- instance (SingI a, SingI b) => SingI (a:.b) where
--     sing = SFrac (fromSing (sing::Sing a)) (fromSing (sing::Sing b))
-- 
-- instance Fractional r => SingE (Kind :: Frac) r where
--     fromSing (SFrac a b) = fromIntegral a + fromIntegral b/(10^^numdigits b)
--         where
--             numdigits b = fromIntegral $ length $ show b
