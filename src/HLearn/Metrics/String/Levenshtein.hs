{-# LANGUAGE OverloadedStrings #-}
module HLearn.Metrics.String.Levenshtein
    where

import GHC.Exts
import Data.Function.Memoize

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

newtype Levenshtein str = Levenshtein str
    deriving (Read,Show,Eq,Ord)

instance IsString (Levenshtein String) where
    fromString = Levenshtein

-------------------------------------------------------------------------------
-- metric space

instance HasRing (Levenshtein String) where
    type Ring (Levenshtein String) = Double

instance MetricSpace (Levenshtein String) where
    distance (Levenshtein xs) (Levenshtein ys) = fromIntegral $ lev xs ys

lev :: String -> String -> Int
lev = memoize2 lev' 
    where
        lev' [] [] = 0
        lev' xs [] = length xs
        lev' [] ys = length ys
        lev' (x:xs) (y:ys) = minimum
            [ lev xs (y:ys) + 1
            , lev (x:xs) ys + 1
            , lev xs ys + if (x/=y) then 1 else 0
            ]
