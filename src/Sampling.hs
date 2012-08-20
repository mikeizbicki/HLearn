{-# LANGUAGE TemplateHaskell #-}

module Sampling
    where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
-- import Data.IORef
-- import Data.Random
-- import Data.Random.Distribution.Poisson
-- import Data.Random.Distribution.Normal
-- import Statistics.Distribution
-- import Statistics.Distribution.Normal
-- import Statistics.Distribution.Poisson
-- import Data.Random.Normal


import Data.Random
import System.Random.MWC (create)


forMtest = do
    let total = 1
    forM_ [1..10] $ \i -> do
        let total = total + i
        return total
        
        
    print total

-- algorithm poisson random number (Knuth):
--     init:
--          Let L ← e−λ, k ← 0 and p ← 1.
--     do:
--          k ← k + 1.
--          Generate uniform random number u in [0,1] and let p ← p × u.
--     while p > L.
--     return k − 1.

    

{-logNormal :: Double -> Double -> RVar Double
logNormal mu sigmaSq = do
    x <- poisson mu -- sigmaSq
    return ( x)

poisson2 :: Float -> RVar Float
poisson2 mu = poisson mu
--     x <- poisson mu
--     return $ x

instance (RandomGen g) => Data.Random.MonadRandom (Rand g) where
--              getRandomDouble = getRandom
--              getRandomWord16 = getRandom
             {- etc... -}

-- test :: (RandomGen g) => Rand g Int
-- test = sample $ poisson (10::Double)

main = do
    mwc <- create
    y <- sampleFrom mwc (logNormal 5 1)
    print y
    
rwalkIO :: IO (RVarT IO Double)
rwalkIO = do
     lastVal <- newIORef 0
     
     let x = do
             prev    <- lift (readIORef lastVal)
             change  <- rvarT StdNormal
             
             let new = prev + change
             lift (writeIORef lastVal new)
             return new
         
     return x-}
-- rejectionSampleDiscrete :: (DiscreteDistr d, RandomGen g,Random i, Integral i) => d -> Rand g i
-- rejectionSampleDiscrete dist = do
--     g <- liftM abs getRandom
--     return g
--     
--     
-- -- -- http://www.wikicoursenote.com/wiki/Acceptance-Rejection_Sampling
-- rejectionSamplePoisson :: (RandomGen g,Random i, Integral i) => PoissonDistribution -> Rand g i
-- rejectionSamplePoisson dist = do
--     -- let g(x) be the normal distribution with mean and stddev same as dist
--     let c = probability (poissonLambda dist) dist
--     y <- liftM abs getRandom
--     u <- getRandomR (0,1) :: Rand g Double
--     if u <= (probability y dist)/(c*(normalDistr 5 5))
--        then y
--        else rejectionSamplePoisson dist
-- --     return y

{-p = do
    x <- sample $ rvar $ Normal 10 2
    return x-}
    
-- normalPair =  do
--      u <- stdUniform
--      t <- stdUniform
--      let r = sqrt (-2 * log u)
--          theta = (2 * pi) * t
--          
--          x = r * cos theta
--          y = r * sin theta
--      return (x,y)