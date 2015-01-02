#!/usr/bin/env runghc

import Data.List.Split
import System.Environment

main = do
    str:[] <- getArgs
    print $ str2seconds str

str2seconds :: String -> Double
str2seconds xs = case splitOn ":" xs of
    [s] -> read s
    [m,s] -> read m*60 + read s
    [h,m,s] -> read h*60*60+read m*60+read s

