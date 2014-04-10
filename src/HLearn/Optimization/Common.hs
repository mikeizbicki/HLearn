module HLearn.Optimization.Common
    ( optimize
    , runOptimization
    , unsafeRunOptimization

    -- * stopping conditions
    , maxIterations
    , lowerBound
    , fx1grows
    , multiplicativeTollerance

    -- * data membership classes
    , Has_x1 (..)
    , Has_fx1 (..)
    , Has_fx0 (..)
    , Has_f'x1 (..)
    , Has_stepSize (..)
    , module HLearn.Algebra.History
    )
    where

import Data.Dynamic
import Data.Typeable
import Debug.Trace

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

-- maxIterations :: (Typeable a) => Int -> a -> History Bool
maxIterations :: Int -> a -> History Bool
maxIterations i opt =  do
    itr <- countEvents
    return $ itr > i 

lowerBound :: 
    ( Has_fx1 opt v
    , Ord (Scalar v)
    )  => Scalar v -> opt v -> History Bool
lowerBound threshold opt = return $ fx1 opt < threshold

fx1grows :: 
    ( Has_fx1 opt v
    , Ord (Scalar v)
    , Typeable opt
    , Typeable v
    , Show (Scalar v)
    ) => opt v -> History Bool
fx1grows opt1 = do
    mfx0 <- get_fx0 opt1
    return $ case mfx0 of
        Nothing -> False
        Just fx0 -> fx0 < fx1 opt1
 
get_fx0 :: forall opt v.
    ( Has_fx1 opt v
    , Typeable opt
    , Typeable v
    ) => opt v -> History (Maybe (Scalar v))
get_fx0 opt1 = do
    me <- prevEventOfType (typeOf opt1)
    return $ case me of
        Nothing -> Nothing
        Just e -> case fromDynamic (dyn e) :: Maybe (opt v) of
            Just opt0 -> Just $ fx1 opt0
            Nothing -> error "get_fx0: this should never happen"
    

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
            report opt
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
