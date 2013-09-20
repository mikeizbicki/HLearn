{-# LANGUAGE OverloadedStrings #-}
module HLearn.Metrics.Hamming
    where

import GHC.Exts

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype Hamming str = Hamming str
    deriving (Read,Show,Eq,Ord)

instance IsString (Hamming String) where
    fromString = Hamming

-------------------------------------------------------------------------------
-- metric space

instance HasRing (Hamming String) where
    type Ring (Hamming String) = Double

instance MetricSpace (Hamming String) where
    distance (Hamming xs) (Hamming ys) = fromIntegral $ go xs ys 0
        where
            go :: String -> String -> Int -> Int
            go [] [] i = i
            go xs [] i = i + length xs
            go [] ys i = i + length ys
            go (x:xs) (y:ys) i = go xs ys $ i + if x==y
                then 0
                else 1

