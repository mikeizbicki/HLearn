{-# LANGUAGE DataKinds,TypeOperators #-}
module HLearn.Algebra.Types.Frac
    where

import GHC.TypeLits

---------------------------------------

data Frac = Frac Nat Nat 

data instance Sing (n::Frac) = SFrac Integer Integer

instance (SingI a, SingI b) => SingI ('Frac a b) where
    sing = SFrac (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))

instance Fractional r => SingE (Kind :: Frac) r where
    fromSing (SFrac a b) = fromIntegral a/fromIntegral b

type (/) a b = 'Frac a b

-- data Frac = (/) Nat Nat 
-- 
-- data instance Sing (n::Frac) = SFrac Integer Integer
-- 
-- instance (SingI a, SingI b) => SingI ((/) a b) where
--     sing = SFrac (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))
-- 
-- instance Fractional r => SingE (Kind :: Frac) r where
--     fromSing (SFrac a b) = fromIntegral a/fromIntegral b
