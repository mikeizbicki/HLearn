{-# LANGUAGE DeriveDataTypeable,TemplateHaskell,DataKinds #-}
-- | A great introduction is: "An introduction to the conjugate gradient method without the pain" by Jonathan Richard Shewchuk
--
-- NOTE: Does this paper has a mistake in the calculation of the optimal step size; it should be multiplied by 1/2?
module HLearn.Optimization.GradientDescent
    ( conjugateGradientDescent
    , conjugateGradientDescent_

    -- ** conjugate method
--     , ConjugateMethod
    , steepestDescent
    , fletcherReeves
    , polakRibiere
    , hestenesStiefel

    -- ** Internal use only
    , ConjugateGradientDescent (..)
--     , linsolvepsd
    )
    where

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)

import HLearn.History
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate

import Debug.Trace

-------------------------------------------------------------------------------
-- data types

data ConjugateGradientDescent a = ConjugateGradientDescent
    { __x1      :: !a
    , __fx1     :: !(Scalar a)
    , __f'x1    :: !a
    , __alpha   :: !(Scalar a)
    , __f'x0    :: !a
    , __s0      :: !a
    , __f       :: !(a -> Scalar a)
    }
    deriving (Typeable)
makeLenses ''ConjugateGradientDescent

instance Has_f ConjugateGradientDescent v where flens = _f

instance Has_x1 ConjugateGradientDescent v where x1 = _x1
instance Has_fx1 ConjugateGradientDescent v where fx1 = _fx1
instance Has_f'x1 ConjugateGradientDescent v where f'x1 = _f'x1
instance Has_stepSize ConjugateGradientDescent v where stepSize = _alpha

---------------------------------------

-- | method for determining the conjugate direction; See <https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient>
type ConjugateMethod = forall v. InnerProductSpace v => v -> v -> v -> Scalar v

{-# INLINABLE steepestDescent #-}
steepestDescent :: ConjugateMethod
steepestDescent _ _ _ = 0

{-# INLINABLE fletcherReeves #-}
fletcherReeves :: ConjugateMethod
fletcherReeves f'x1 f'x0 _ = f'x1 <> f'x1 / f'x0 <> f'x0

{-# INLINABLE polakRibiere #-}
polakRibiere :: ConjugateMethod
polakRibiere f'x1 f'x0 _ = (f'x1 <> (f'x1 - f'x0)) / (f'x0 <> f'x0)

{-# INLINABLE hestenesStiefel #-}
hestenesStiefel :: ConjugateMethod
hestenesStiefel f'x1 f'x0 s0 = -(f'x1 <> (f'x1 - f'x0)) / (s0 <> (f'x1 - f'x0))

-------------------

-- type MultivariateLineSearch m v =
--     (v -> Scalar v) -> (v -> v) -> v -> v -> Scalar v -> m (Scalar v)


-------------------------------------------------------------------------------
-- functions

-- | Conjugate gradient descent with reasonable default parameters
conjugateGradientDescent ::
    ( InnerProductSpace v
    , BoundedField (Scalar v)
    , HistoryMonad m
    , Reportable m (Scalar v)
    , Reportable m (LineBracket (Scalar v))
    , Reportable m (Brent (Scalar v))
    , Reportable m (ConjugateGradientDescent v)
    , Show (Scalar v)
    , Show v
    , Logic v~Bool
    ) => (v -> Scalar v)
      -> (v -> v)
      -> v
      -> m (ConjugateGradientDescent v)
conjugateGradientDescent f f' x0 =
    trace ("x0="++show x0) $
    conjugateGradientDescent_
        ( lineSearchBrent ( brentTollerance 1e-12 || maxIterations 20 ))
        polakRibiere
        f f' x0
        ( maxIterations 20 || multiplicativeTollerance 1e-6 )


-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
conjugateGradientDescent_ ::
    ( InnerProductSpace v
    , HistoryMonad m
    , Reportable m (ConjugateGradientDescent v)
    ) => MultivariateLineSearch m v
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> v
      -> StopCondition m (ConjugateGradientDescent v)
      -> m  (ConjugateGradientDescent v)
conjugateGradientDescent_ searchMethod conjugateMethod f f' x0 = {-# SCC conjugateGradientDescent #-} optimize
    (step_conjugateGradientDescent searchMethod conjugateMethod f f')
    $ ConjugateGradientDescent
        { __x1 = x0
        , __fx1 = f x0
        , __f'x1 = f' x0
        , __alpha = 1e-2
        , __f'x0 = 2 *. f' x0
        , __s0 = f' x0
        , __f = f
        }

-- | performs a single iteration of the conjugate gradient descent algorithm
step_conjugateGradientDescent ::
    ( InnerProductSpace v
    , HistoryMonad m
    ) => MultivariateLineSearch m v
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> ConjugateGradientDescent v
      -> m (ConjugateGradientDescent v)
step_conjugateGradientDescent stepMethod conjMethod f f' (ConjugateGradientDescent x1 fx1 f'x1 alpha1 f'x0 s0 _) = {-# SCC step_conjugateGradientDescent #-} do
    -- this test ensures that conjugacy will be reset when it is lost; the
    -- formula is taken from equation 1.174 of the book "Nonlinear Programming"

--     let beta = if abs (f'x1 <> f'x0) > 0.2 * squaredInnerProductNorm f'x0
--             then 0
--             else max 0 $ conjMethod f'x1 f'x0 s0

    let beta = conjMethod f'x1 f'x0 s0

    let s1 = - f'x1 + beta *. f'x1
--     let s1 = - f'x1 + beta *. s0

    alpha <- stepMethod f f' x1 s1 alpha1

    let x = x1 + alpha *. s1

    return $ ConjugateGradientDescent
        { __x1 = x
        , __fx1 = f x
        , __f'x1 = f' x
        , __alpha = alpha
        , __f'x0 = f'x1
        , __s0 = s1
        , __f = f
        }

