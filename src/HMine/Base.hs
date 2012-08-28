{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HMine.Base
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies
import Data.Number.LogFloat
import Data.Semigroup
import Data.List


-------------------------------------------------------------------------------
-- HMine monad

type HMine a = Rand StdGen a

runHMine :: Int -> (HMine a) -> a
runHMine seed hmine = evalRand hmine (mkStdGen seed)

-- instance (Semigroup a) => Semigroup (HMine a) where
--     (<>) a1 a2 = do
--         seed1 <- getRandom
--         seed2 <- getRandom
--         return $ (runHMine seed1 a1) <> (runHMine seed2 a2)

instance (Semigroup a, NFData a) => Semigroup (HMine a) where
    (<>) a1 a2 = do
        seed1 <- getRandom
        seed2 <- getRandom
        return $ runEval $ do
            a1' <- rpar $ runHMine seed1 a1
            a2' <- rseq $ runHMine seed2 a2
            return $ a1' <> a2'

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
        
-------------------------------------------------------------------------------
-- From the hstats package

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Sample variance
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)
           
-- |Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs