module HMine.Base
    where

import Control.Monad.Random
import Data.Number.LogFloat


-------------------------------------------------------------------------------
-- HMine monad

type HMine a = Rand StdGen a

runHMine :: Int -> (HMine a) -> a
runHMine seed hmine = evalRand hmine (mkStdGen seed)

-------------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

type Probability = LogFloat

-------------------------------------------------------------------------------
-- bool <-> int

indicator :: (Num a) => Bool -> a
indicator b = 
    if b
        then 1
        else 0                 

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