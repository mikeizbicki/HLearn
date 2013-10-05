{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

import Criterion.Config
import Criterion.Main

import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Metrics.Lebesgue
    
myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 100
    }
    
dp1 = L2 $ VU.fromList [0,1,2,3,4,5::Double]
dp2 = L2 $ VU.fromList [1,2,2,3,5,3::Double]

main = defaultMainWith myConfig (return ())
    [ bench "1" $ nf (distance dp1) dp2
    , bench "2" $ nf (distance1 dp1) dp2
    , bench "3" $ nf (distance2 dp1) dp2
    ]

{-# INLINABLE distance1 #-}
distance1 :: L2 (VU.Vector Double) -> L2 (VU.Vector Double) -> Double
-- distance1 !(L2 v1) !(L2 v2) = sqrt $ VU.foldl1' (+) $ VU.zipWith (\a b -> (a-b)*(a-b)) v1 v2
distance1 !(L2 v1) !(L2 v2) = sqrt $ VU.foldl' (\r (a,b) -> (a-b)+r) 0 $ VU.zip v1 v2

{-# INLINABLE distance2 #-}
distance2 :: L2 (VU.Vector Double) -> L2 (VU.Vector Double) -> Double
distance2 !(L2 v1) !(L2 v2) = {-# SCC distance #-} sqrt $ go 0 (VU.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                          *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)) (i-1)

{-# INLINABLE isFartherThan2 #-}
isFartherThan2 :: L2 (VU.Vector Double) -> L2 (VU.Vector Double) -> Double -> Bool
isFartherThan2 !(L2 v1) !(L2 v2) !dist = go 0 (VU.length v1-1) 
    where
        dist2=dist*dist

        go tot (-1) = False 
        go tot i = if tot'>dist2
            then True
            else go tot' (i-1)
            where
                tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
                          *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)

{-# INLINABLE isFartherThan3 #-}
isFartherThan3 :: L2 (VU.Vector Double) -> L2 (VU.Vector Double) -> Double -> Bool
isFartherThan3 !(L2 v1) !(L2 v2) !dist = go 0 (VU.length v1-1) 
    where
--         dist2=dist*dist

        go :: Double -> Int -> Bool
        go !tot !i
--             | () !tot !i !False = undefined
            | i== (-1) = False
            | otherwise = case tot'>dist*dist of
                True -> True
                False -> go tot' (i-1)
            where
                tot' = tot+(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
--                           *(v1 `VU.unsafeIndex` i-v2 `VU.unsafeIndex` i)
