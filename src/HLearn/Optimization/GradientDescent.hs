{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, RebindableSyntax #-}
module HLearn.Optimization.GradientDescent
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
import Control.Monad.Writer
import Data.Dynamic
import Data.List
import Data.List.Extras
import Data.Typeable
import Debug.Trace
import qualified Data.DList as DList
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
-- import HLearn.Algebra.Structures.Monad
import HLearn.Optimization.Common
import qualified HLearn.Optimization.LineMinimization as LineMin

-------------------------------------------------------------------------------
-- data types

data ConjugateGradientDescent a = ConjugateGradientDescent 
    { _x1 :: !a
    , _fx1 :: !(Scalar a)
    , _f'x1 :: !a
    , _alpha :: !(Scalar a)
    , _f'x0 :: !a
    , _s0 :: !a
    }
    deriving (Typeable)

-- deriving instance (Read a, Read (Scalar a)) => Read (ConjugateGradientDescent a)
-- deriving instance (Show a, Show (Scalar a)) => Show (ConjugateGradientDescent a)

instance Has_x1 ConjugateGradientDescent v where x1 = _x1
instance Has_fx1 ConjugateGradientDescent v where fx1 = _fx1
instance Has_f'x1 ConjugateGradientDescent v where f'x1 = _f'x1

---------------------------------------

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

-------------------------------------------------------------------------------
-- functions

-- | Conjugate gradient descent using reasonable defaults for the optimization parameters.  This is the recommended function to use.
-- conjugateGradientDescent ::
--     ( InnerProduct v
--     , Ord (Scalar v)
--     , Show (Scalar v)
--     ) => (v -> Scalar v)
--       -> (v -> v)
--       -> v
--       -> DoTrace (ConjugateGradientDescent v)
conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch FletcherReeves f f'
-- conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch PolakRibiere f f'
-- conjugateGradientDescent = conjugateGradientDescent_ LineSearch PolakRibiere

-- | The method of steepest descent is much worse in practice than conjugate gradient descent.  It should never be used in practice, and is provided only for comparison purposes.
-- steepestDescent :: 
--     ( InnerProduct v
--     , Ord (Scalar v)
--     , Show (Scalar v)
--     ) => (v -> Scalar v)
--       -> (v -> v)
--       -> v
--       -> DoTrace (ConjugateGradientDescent v)
steepestDescent f f' = conjugateGradientDescent_ LineSearch None f f'

-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
-- conjugateGradientDescent_ ::
--     ( InnerProduct v
--     , Ord (Scalar v)
--     , Typeable (Scalar v)
--     , Typeable v
--     , Show (Scalar v)
--     ) => StepMethod (Scalar v)
--       -> ConjugateMethod
--       -> (v -> Scalar v)
--       -> (v -> v)
--       -> v
--       -> History (ConjugateGradientDescent v)
conjugateGradientDescent_ searchMethod conjugateMethod f f' x0 = optimize
    (step_conjugateGradientDescent searchMethod conjugateMethod f f')
    $ ConjugateGradientDescent
        { _x1 = x0
        , _fx1 = f x0
        , _f'x1 = f' x0
        , _alpha = 1e-2
        , _f'x0 = f' x0
        , _s0 = f' x0
        }

-- | performs a single iteration of the conjugate gradient descent algorithm
step_conjugateGradientDescent :: 
    ( InnerProduct v
    , Ord (Scalar v)
    , Show (Scalar v)
    , Typeable (Scalar v)
    , Scalar (Scalar v) ~ Scalar v
    , Typeable v
    ) => StepMethod (Scalar v)
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> ConjugateGradientDescent v
      -> History (ConjugateGradientDescent v)
step_conjugateGradientDescent stepMethod conjMethod f f' (ConjugateGradientDescent x1 fx1 f'x1 alpha1 f'x0 s0) = do
    let beta = max 0 $ case conjMethod of
            None -> 0
            FletcherReeves -> inner f'x1 f'x1 / inner f'x0 f'x0
            PolakRibiere -> (inner f'x1 (f'x1 <> inverse f'x0)) / (inner f'x0 f'x0)
            HestenesStiefel -> -(inner f'x1 (f'x1 <> inverse f'x0)) / (inner s0 (f'x1 <> inverse f'x0))

    let s1 = inverse f'x1 <> beta .* f'x1

    let g y = f $ x1 <> y .* s1
        g' y = undefined -- f' (x1 <> y .* sl) 

    alpha <- case stepMethod of
        StepSize x -> return x
        LineSearch -> do
            bracket <- LineMin.lineBracket g (alpha1/2) (alpha1*2)
            brent <- LineMin.brent g bracket [LineMin.brentTollerance 1e-6]
--             brent <- LineMin.brent g bracket [maxIterations 100, LineMin.brentTollerance 1e-6]
            return $ LineMin._x brent
--             gss <- LineMin.goldenSectionSearch g $ curValue bracket
--             return $ LineMin.getgss $ curValue $ gss

    let x = x1 <> alpha .* s1

    report $ ConjugateGradientDescent
        { _x1 = x
        , _fx1 = f x
        , _f'x1 = f' x
        , _alpha = alpha
        , _f'x0 = f'x1
        , _s0 = s1
        }


