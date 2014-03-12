module HLearn.Numeric.Recipes.GradientDescent
    ( conjugateGradientDescent
    , steepestDescent
    , conjugateGradientDescent_
    , ConjugateGradientDescent (..)
    , StepMethod (..)
    , ConjugateMethod (..)
    )
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Data.List
import Data.List.Extras
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Numeric.Recipes
import qualified HLearn.Numeric.Recipes.LineMin as LineMin


data ConjugateGradientDescent a = ConjugateGradientDescent 
    { ___x1 :: !a
    , ___fx1 :: !(Scalar a)
    , ___f'x1 :: !a
    , ___f'x0 :: !a
    , ___s0 :: !a
    }

instance Has_x1 ConjugateGradientDescent where
    x1 = ___x1

-- | Selects a method for choosing the step size
data StepMethod r
    = StepSize r
    | LineSearch

-- | method for determining the conjugate direction; See <https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient>
data ConjugateMethod
    = None
    | FletcherReeves
    | PolakRibiere
    | HestenesStiefel

-- | Conjugate gradient descent using reasonable defaults for the optimization parameters.  This is the recommended function to use.
conjugateGradientDescent ::
    ( InnerProduct v
    , Ord (Scalar v)
    , Show (Scalar v)
    ) => (v -> Scalar v)
      -> (v -> v)
      -> v
      -> DoTrace (ConjugateGradientDescent v)
conjugateGradientDescent = conjugateGradientDescent_ LineSearch PolakRibiere

-- | The method of steepest descent is much worse in practice than conjugate gradient descent.  It should never be used in practice, and is provided only for comparison purposes.
steepestDescent :: 
    ( InnerProduct v
    , Ord (Scalar v)
    , Show (Scalar v)
    ) => (v -> Scalar v)
      -> (v -> v)
      -> v
      -> DoTrace (ConjugateGradientDescent v)
steepestDescent = conjugateGradientDescent_ LineSearch None

-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
conjugateGradientDescent_ ::
    ( InnerProduct v
    , Ord (Scalar v)
    , Show (Scalar v)
    ) => StepMethod (Scalar v)
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> v
      -> DoTrace (ConjugateGradientDescent v)
conjugateGradientDescent_ searchMethod conjugateMethod f f' x0 = optimize
    (_stop_itr 1000 <> _stop_tolerance ___fx1 1e-6)
    (step_conjugateGradientDescent searchMethod conjugateMethod f f')
    (initTrace cgd1 cgd0)
    where
        cgd1 = step_conjugateGradientDescent LineSearch None f f' cgd0

        cgd0 = ConjugateGradientDescent
            { ___x1 = x0
            , ___fx1 = f x0
            , ___f'x1 = f' x0
            , ___f'x0 = f' x0
            , ___s0 = f' x0
            }

-- | performs a single iteration of the conjugate gradient descent algorithm
step_conjugateGradientDescent :: 
    ( InnerProduct v
    , Ord (Scalar v)
    , Show (Scalar v)
    ) => StepMethod (Scalar v)
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> ConjugateGradientDescent v
      -> ConjugateGradientDescent v
step_conjugateGradientDescent stepMethod conjMethod f f' (ConjugateGradientDescent x1 fx1 f'x1 f'x0 s0) = 
  trace ("fx1="++show fx1++"; beta="++show beta++"; alpha="++show alpha) $ ConjugateGradientDescent
    { ___x1 = x
    , ___fx1 = f x
    , ___f'x1 = f' x
    , ___f'x0 = f'x1
    , ___s0 = s1
    }
    where
        beta = max 0 $ case conjMethod of
            None -> 0
            FletcherReeves -> inner f'x1 f'x1 / inner f'x0 f'x0
            PolakRibiere -> (inner f'x1 (f'x1 <> inverse f'x0)) / (inner f'x0 f'x0)
            HestenesStiefel -> -(inner f'x1 (f'x1 <> inverse f'x0)) / (inner s0 (f'x1 <> inverse f'x0))

--             FletcherReeves -> (sumElements $ f'x1 * f'x1) / (sumElements $ f'x0 * f'x0)
--             PolakRibiere -> (sumElements $ f'x1*(f'x1-f'x0)) / (sumElements $ f'x0 * f'x0)
--             HestenesStiefel -> -(sumElements $ f'x1*(f'x1-f'x0)) / (sumElements $ s0 * (f'x1-f'x0))

        s1 = inverse f'x1 <> beta .* f'x1
        g y = f $ x1 <> y .* s1

        alpha = case stepMethod of
            StepSize x -> x
            LineSearch -> LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1)

        x = x1 <> alpha .* s1

