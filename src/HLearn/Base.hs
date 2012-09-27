{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HLearn.Base
    ( HLearn, runHLearn
    , fi
    , Probability
    , indicator, bool2num, num2bool
    , histogram, normalizeL
    , module HLearn.Algebra
    , module Control.Monad
    , module Control.Monad.Random
    , module Data.Binary
    , module Data.Hashable
    , module Data.Number.LogFloat
    )
    where

import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.Hashable
import Data.Number.LogFloat hiding (log)
import Data.List

import qualified Data.Map as Map

import HLearn.Algebra

-------------------------------------------------------------------------------
-- HLearn monad

type HLearn a = Rand StdGen a

runHLearn :: Int -> (HLearn a) -> a
runHLearn seed hmine = evalRand hmine (mkStdGen seed)

instance (Semigroup a) => Semigroup (HLearn a) where
    (<>) a1 a2 = do
        seed1 <- getRandom
        seed2 <- getRandom
        return $ (runHLearn seed1 a1) <> (runHLearn seed2 a2)

-- instance (Semigroup a, NFData a) => Semigroup (HLearn a) where
--     (<>) a1 a2 = do
--         seed1 <- getRandom
--         seed2 <- getRandom
--         return $ runEval $ do
--             a1' <- rpar $ runHLearn seed1 a1
--             a2' <- rseq $ runHLearn seed2 a2
--             return $ a1' <> a2'

-------------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

type Probability = LogFloat
---------------------------------------

instance Read LogFloat where
    readsPrec = error "LogFloat.read: not implemented"

instance Binary LogFloat where
    put = error "LogFloat.put: not implemented"
    get = error "LogFloat.get: not implemented"

instance Hashable LogFloat where
    hash = error "LogFloat.hash: not implemented"

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
-- Just some list functions

histogram :: (Ord label) => [label] -> [(label,Int)]
histogram = Map.assocs . go Map.empty {-. map (\x -> (x,1))-}
    where
        go :: (Ord label) => Map.Map label Int -> [label] -> Map.Map label Int
        go !m ![]     = m
        go !m !(x:xs) = go (Map.insertWith (+) x 1 m) xs


normalizeL :: (Fractional a) => [a] -> [a]
normalizeL xs = map (/s) xs
    where
        s = sum xs


-------------------------------------------------------------------------------
-- From the hstats package

-- -- |Numerically stable mean
-- mean :: Floating a => [a] -> a
-- mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x
-- 
-- -- |Sample variance
-- var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
--     where
--       var' _ _ s [] = s
--       var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
--          where
--            delta = x - m
--            nm = m + delta/(fromIntegral $ n + 1)
--            
-- -- |Standard deviation of sample
-- stddev :: (Floating a) => [a] -> a
-- stddev xs = sqrt $ var xs