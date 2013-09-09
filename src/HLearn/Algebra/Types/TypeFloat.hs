{-# LANGUAGE DataKinds #-}
module HLearn.Algebra.Types.TypeFloat
    where

import GHC.TypeLits

data TypeFloat = (%) Nat Nat 

data instance Sing (n::TypeFloat) = SFloat Integer Integer

instance (SingI a, SingI b) => SingI ((%) a b) where
    sing = SFloat (fromSing (sing :: Sing a)) (fromSing (sing :: Sing b))

instance Fractional r => SingE (Kind :: TypeFloat) r where
    fromSing (SFloat a b) = fromIntegral a/fromIntegral b
