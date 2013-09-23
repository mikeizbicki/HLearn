module HLearn.Algebra.Common
    where

indicator :: Bool -> Int
indicator True = 1
indicator False = 0

bool2int :: Bool -> Int
bool2int True = 1
bool2int False = -1

infinity :: Fractional a => a
infinity = 1/0


