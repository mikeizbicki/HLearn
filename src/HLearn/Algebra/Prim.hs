{-# LANGUAGE MagicHash,UnboxedTuples #-}

module HLearn.Algebra.Prim
    where

{-# INLINE fst# #-}
fst# :: (# a, b #) -> a
fst# (# a, b #) = a

{-# INLINE snd# #-}
snd# :: (# a, b #) -> b
snd# (# a, b #) = b

