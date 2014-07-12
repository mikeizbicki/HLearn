module HLearn.Algebra.Structures.CanError
    where

import qualified Data.Strict.Maybe as Strict

class CanError a where
    errorVal :: a
    isError :: a -> Bool

instance CanError (Maybe a) where
    {-# INLINE isError #-}
    isError Nothing = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Nothing

instance CanError (Strict.Maybe a) where
    {-# INLINE isError #-}
    isError Strict.Nothing = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Strict.Nothing

instance CanError Float where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
--     isError a = a /= a 
    errorVal = 0/0

instance CanError Double where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
--     isError a = a /= a 
    errorVal = 0/0
