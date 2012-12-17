module Parallelism
    where

import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import Data.List
import Data.Semigroup
 
cutoff = 35
 
fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)
 
fib :: Int -> Integer
fib n | n < cutoff = fib' n
      | otherwise  = r `par` (l `pseq` l + r)
 where
    l = fib (n-1)
    r = fib (n-2)
    
reduce :: (Semigroup sg) => [sg] -> sg
reduce = foldl1' (<>)