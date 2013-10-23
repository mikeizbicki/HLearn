module HLearn.Algebra.Common
    where

indicator :: Num a => Bool -> a
indicator True = 1
indicator False = 0

-- bool2int :: Bool -> Int
-- bool2int True = 1
-- bool2int False = -1

bool2num :: (Num a) => Bool -> a
bool2num b = 
    if b
        then 1
        else -1

num2bool :: (Ord a, Num a) => a -> Bool
num2bool a = 
    if a<0
        then False
        else True 

infinity :: Fractional a => a
infinity = 1/0


