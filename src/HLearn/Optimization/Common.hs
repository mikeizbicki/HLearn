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

import SubHask

import HLearn.History

-------------------------------------------------------------------------------

class LearningRate init step v | init -> step, step -> init where
    lrStep  :: HistoryMonad m => init v -> step v -> v -> m (step v)
    lrInit  :: init v -> v -> step v
    lrApply :: init v -> step v -> v -> v

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

{-# INLINE optimize #-}
optimize :: (HistoryMonad m, Reportable m opt)
    => (opt -> m opt)               -- ^ step function
    -> opt                          -- ^ initial conditions
    -> StopCondition m opt          -- ^ stop conditions
    -> m opt
optimize step opt0 stop = {-# SCC optimize #-} collectReports $ do
    report opt0
    report opt0
    opt1 <- step opt0
    go opt0 opt1
    where
        go prevopt curopt = {-# SCC optimize_go #-} do
            report curopt
            done <- stop prevopt curopt
            if done
                then return curopt
                else do
                    opt' <- step curopt
                    go curopt opt'

type StopCondition m opt = (Boolean (m Bool), HistoryMonad m) => opt -> opt -> m Bool

maxIterations :: HistoryMonad m => Int -> StopCondition m opt
maxIterations i _ _ = {-# SCC maxIterations #-} do
    t <- currentItr
    return $ t >= i

lowerBound ::
    ( Has_fx1 opt v
    , Ord (Scalar v)
    , HistoryMonad m
    )  => Scalar v -> StopCondition m (opt v)
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
