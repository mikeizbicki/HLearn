module HLearn.Optimization.Common
    ( optimize
    , runOptimization
    , unsafeRunOptimization
    , maxIterations
    , multiplicativeTollerance
    , Has_x1 (..)
    , Has_fx1 (..)
    , Has_fx0 (..)
    , Has_f'x1 (..)
    , Has_stepSize (..)
    , module HLearn.Algebra.History
    )
    where

import Data.Typeable

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.Algebra.History

-------------------------------------------------------------------------------

class Has_x1 opt v where x1 :: opt v -> v
class Has_fx1 opt v where fx1 :: opt v -> Scalar v
class Has_fx0 opt v where fx0 :: opt v -> Scalar v
class Has_f'x1 opt v where f'x1 :: opt v -> v
class Has_stepSize opt v where stepSize :: opt v -> Scalar v

---------------------------------------

maxIterations :: (Show a, Typeable a) => Int -> a -> History Bool
maxIterations i opt =  do
    itr <- countEvents
    return $ itr > i 

multiplicativeTollerance :: 
    ( Has_fx1 opt v
    , Has_fx0 opt v
    , Ord (Scalar v)
    , Fractional (Scalar v)
    , Typeable opt 
    , Typeable v
    ) => Scalar v -> opt v -> History Bool
multiplicativeTollerance tol opt = return $ left <= right
    where
        left = 2*abs (fx1 opt - fx0 opt)
        right = tol*(abs (fx1 opt) + abs (fx0 opt) + 1e-18)

-------------------------------------------------------------------------------

optimize :: forall opt.
    ( Typeable opt
    ) => (opt -> History opt)       -- ^ step function
      -> opt                        -- ^ initial conditions
      -> [opt -> History Bool]      -- ^ stopping conditions
      -> History opt
optimize step opt0 stop = collectEvents $ do
    report opt0
    opt1 <- step opt0
    go opt1
    where
        go :: opt -> History opt
        go opt = do
            done <- fmap or $ sequence $ map ($opt) stop  
            if done
                then return opt
                else do
                    opt' <- step opt
                    go opt' 

---------------------------------------

runOptimization :: (Has_x1 opt v) => History (opt v) -> IO (v, [Event])
runOptimization m = do
    (a,log) <- runHistory m
    return (x1 a, log)

unsafeRunOptimization :: Has_x1 opt v => History (opt v) -> (v, [Event])
unsafeRunOptimization m = (x1 a, log)
    where
        (a,log) = unsafeRunHistory m
