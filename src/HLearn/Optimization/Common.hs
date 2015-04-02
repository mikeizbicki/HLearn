{-# LANGUAGE DataKinds #-}
module HLearn.Optimization.Common
--     ( iterate
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

import SubHask

import HLearn.History

-------------------------------------------------------------------------------

-- class LearningRate init step v | init -> step, step -> init where
--     lrStep  :: init v -> step v -> v -> History (step v)
--     lrInit  :: init v -> v -> step v
--     lrApply :: init v -> step v -> v -> v

class Has_f opt v           where flens     :: Lens' (opt v) (v -> Scalar v)

class Has_x1 opt v          where x1        :: Lens' (opt v) v
class Has_fx1 opt v         where fx1       :: Lens' (opt v) (Scalar v)
class Has_fx0 opt v         where fx0       :: Lens' (opt v) (Scalar v)
class Has_f'x1 opt v        where f'x1      :: Lens' (opt v) v
class Has_stepSize opt v    where stepSize  :: Lens' (opt v) (Scalar v)

-- class Has_x1 opt v where x1 :: opt v -> v
-- class Has_x1L opt v where x1L :: Lens' opt v
-- class Has_fx1 opt v where fx1 :: opt v -> Scalar v
-- class Has_fx0 opt v where fx0 :: opt v -> Scalar v
-- class Has_f'x1 opt v where f'x1 :: opt v -> v
-- class Has_stepSize opt v where stepSize :: opt v -> Scalar v

---------------------------------------

type StopCondition = forall a. a -> a -> History Bool
type StopCondition_ a = a -> a -> History Bool

{-# INLINE iterate #-}
iterate :: forall a. Optimizable a
    => (a -> History a)         -- ^ step function
    -> a                        -- ^ start parameters
    -> StopCondition_ a         -- ^ stop conditions
    -> History a
iterate step opt0 stop = {-# SCC iterate #-} do
    report opt0
    opt1 <- step opt0
    go opt0 opt1
    where
        go prevopt curopt = {-# SCC iterate_go #-} do
            report curopt
            done <- stop prevopt curopt
            if done
                then return curopt
                else do
                    opt' <- step curopt
                    go curopt opt'


maxIterations :: Int -> StopCondition
maxIterations i _ _ = {-# SCC maxIterations #-} do
    t <- getNumReports
    return $ t >= i

lowerBound ::
    ( Has_fx1 opt v
    , Ord (Scalar v)
    )  => Scalar v -> StopCondition_ (opt v)
lowerBound threshold _ opt = {-# SCC lowerBound #-} return $ opt^.fx1 < threshold

-- fx1grows ::
--     ( Has_fx1 opt v
--     , Ord (Scalar v)
--     , Typeable opt
--     , Typeable v
--     ) => StopCondition (opt v)
-- fx1grows opt1 = {-# SCC fx1grows #-} do
--     mfx0 <- get_fx0 opt1
--     return $ case mfx0 of
--         Nothing -> False
--         Just fx0 -> fx0 < opt1^.fx1

-- get_fx0 :: forall opt v m.
--     ( Has_fx1 opt v
--     , HistoryMonad m
--     , Typeable opt
--     , Typeable v
--     ) => opt v -> m (Maybe (Scalar v))
-- get_fx0 opt1 = {-# SCC get_fx0 #-} do
--     prev <- prevReport
--     return $ case fromDynamic (dyn prev) :: Maybe (opt v) of
--         Just opt0 -> Just $ opt0^.fx1
--         Nothing -> error "get_fx0: this should never happen"

-- multiplicativeTollerance :: forall opt v.
--     ( Field (Scalar v)
--     , Normed (Scalar v)
--     , Ord (Scalar v)
--     , Has_fx1 opt v
--     , HasScalar v
--     , Typeable (opt v)
--     ) => Scalar v -> StopCondition (opt v)
multiplicativeTollerance tol prevopt curopt = {-# SCC multiplicativeTollerance #-} do
    return $ prevopt^.fx1 /= infinity && left < right
        where
            left = 2*abs (curopt^.fx1 - prevopt^.fx1)
            right = tol*(abs (curopt^.fx1) + abs (prevopt^.fx1) + 1e-18)
