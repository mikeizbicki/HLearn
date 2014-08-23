{-# LANGUAGE DataKinds #-}
module HLearn.Optimization.Common
--     ( optimize
--     , runOptimization
--     , unsafeRunOptimization
-- 
--     -- * stopping conditions
--     , maxIterations
--     , lowerBound
--     , fx1grows
--     , multiplicativeTollerance
-- 
--     -- * data membership classes
--     , Has_x1 (..)
--     , Has_fx1 (..)
--     , Has_fx0 (..)
--     , Has_f'x1 (..)
--     , Has_stepSize (..)
--     , module HLearn.Algebra.History
--     )
    where

import Control.Lens
import Data.Dynamic
import Data.Typeable
import Debug.Trace

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.Algebra.LinearAlgebra
import HLearn.History

-------------------------------------------------------------------------------

class ValidTensor1 v => Has_f opt v           where flens     :: Lens' (opt v) (Tensor 1 v -> Tensor 0 v)

class ValidTensor1 v => Has_x1 opt v          where x1        :: Lens' (opt v) (Tensor 1 v)
class ValidTensor1 v => Has_fx1 opt v         where fx1       :: Lens' (opt v) (Tensor 0 v)
class ValidTensor1 v => Has_fx0 opt v         where fx0       :: Lens' (opt v) (Tensor 0 v)
class ValidTensor1 v => Has_f'x1 opt v        where f'x1      :: Lens' (opt v) (Tensor 1 v)
class ValidTensor1 v => Has_stepSize opt v    where stepSize  :: Lens' (opt v) (Tensor 0 v)

-- class Has_x1 opt v where x1 :: opt v -> v
-- class Has_x1L opt v where x1L :: Lens' opt v
-- class Has_fx1 opt v where fx1 :: opt v -> Scalar v
-- class Has_fx0 opt v where fx0 :: opt v -> Scalar v
-- class Has_f'x1 opt v where f'x1 :: opt v -> v
-- class Has_stepSize opt v where stepSize :: opt v -> Scalar v

---------------------------------------

optimize :: Typeable opt
    => (opt -> History opt)         -- ^ step function
    -> opt                          -- ^ initial conditions
    -> [StopCondition opt]          -- ^ stop conditions
    -> History opt
optimize step init stop = collectReports $ go init
    where
        go opt = do
            report opt
            done <- fmap or $ sequence $ map ($opt) stop  
            if done
                then return opt
                else do 
                    opt' <- step opt
                    go opt'  

type StopCondition opt = opt -> History Bool

maxIterations :: Int -> StopCondition opt
maxIterations i _ = do
    prev <- prevReport
    return $ numReports prev >= i

lowerBound :: 
    ( Has_fx1 opt v
    , Ord (Scalar v)
    )  => Scalar v -> StopCondition (opt v)
lowerBound threshold opt = return $ opt^.fx1 < threshold

fx1grows :: 
    ( Has_fx1 opt v
    , Ord (Scalar v)
    , Typeable opt
    , Typeable v
    ) => StopCondition (opt v)
fx1grows opt1 = do
    mfx0 <- get_fx0 opt1
    return $ case mfx0 of
        Nothing -> False
        Just fx0 -> fx0 < opt1^.fx1
 
get_fx0 :: forall opt v.
    ( Has_fx1 opt v
    , Typeable opt
    , Typeable v
    ) => opt v -> History (Maybe (Scalar v))
get_fx0 opt1 = do
    prev <- prevReport
    return $ case fromDynamic (dyn prev) :: Maybe (opt v) of
        Just opt0 -> Just $ opt0^.fx1
        Nothing -> error "get_fx0: this should never happen"
    
-- multiplicativeTollerance :: 
--     ( Has_fx1 opt v
--     , Ord (Scalar v)
--     , Fractional (Scalar v)
--     , Typeable opt 
--     , Typeable v
--     ) => Scalar v -> StopCondition (opt v)
-- multiplicativeTollerance tol opt1 = do
--     mfx0 <- get_fx0 opt1
--     return $ case mfx0 of
--         Nothing -> False
--         Just fx0 -> trace ("left="++show left++"; right="++show right) $ fx0 /= infinity && left < right
--             where
--                 left = 2*abs (opt1^.fx1 - fx0)
--                 right = tol*(abs (opt1^.fx1) + abs fx0 + 1e-18)

multiplicativeTollerance tol opt = 
    return $ opt^.fx0 /= infinity && left < right
            where
                left = 2*abs (opt^.fx1 - opt^.fx0)
                right = tol*(abs (opt^.fx1) + abs (opt^.fx0) + 1e-18)

-- -- multiplicativeTollerance :: 
-- --     ( Has_fx1 opt v
-- --     , Has_fx0 opt v
-- --     , Ord (Scalar v)
-- --     , Fractional (Scalar v)
-- --     , Typeable opt 
-- --     , Typeable v
-- --     ) => Scalar v -> opt v -> History Bool
-- -- multiplicativeTollerance tol opt = return $ left <= right
-- --     where
-- --         left = 2*abs (opt^.fx1 - opt^.fx0)
-- --         right = tol*(abs (opt^.fx1) + abs (opt^.fx0) + 1e-18)

-------------------------------------------------------------------------------

-- optimize :: forall opt.
--     ( Typeable opt
--     ) => (opt -> History opt)       -- ^ step function
--       -> opt                        -- ^ initial conditions
--       -> [opt -> History Bool]      -- ^ stopping conditions
--       -> History opt
-- optimize step opt0 stop = {-# SCC optimize #-} collectEvents $ do
-- --     report opt0
-- --     opt1 <- step opt0
--     go opt0
--     where
--         go :: opt -> History opt
--         go opt = do
--             done <- fmap or $ sequence $ map ($opt) stop  
--             report opt
--             if done
--                 then return opt
--                 else do
--                     opt' <- step opt
--                     go opt' 

---------------------------------------

-- runOptimization :: 
--     ( Has_x1 opt v
--     , v ~ Tensor 1 v
--     ) => History (opt v) -> IO (v, [Event])
-- runOptimization m = do
--     (a,log) <- runHistory m
--     return (a^.x1, log)
-- 
-- unsafeRunOptimization :: 
--     ( Has_x1 opt v 
--     , v ~ Tensor 1 v
--     ) => History (opt v) -> (v, [Event])
-- unsafeRunOptimization m = (a^.x1, log)
--     where
--         (a,log) = unsafeRunHistory m
