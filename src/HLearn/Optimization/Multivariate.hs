-- | This module provides numerical routines for minimizing linear and nonlinear multidimensional functions.
-- Multidimensional optimization is significantly harder than one dimensional optimization.
-- In general, there are no black box routines that work well on every function,
-- even if the function has only a single local minimum.
--
-- FIXME: better documentation
module HLearn.Optimization.Multivariate
    (

    -- * Simple interface

    -- * Advanced interface

    -- ** Conjugate gradient descent
    -- | A great introduction is:
    -- "An introduction to the conjugate gradient method without the pain" by Jonathan Richard Shewchuk
    --
    -- NOTE: Does this paper has a mistake in the calculation of the optimal step size;
    -- it should be multiplied by 1/2?
      fminunc_cgd_
    , Iterator_cgd

    -- *** Conjugation methods
    , ConjugateMethod
    , steepestDescent
    , fletcherReeves
    , polakRibiere
    , hestenesStiefel

    -- ** Newton-Raphson (quadratic gradient descent)
    , Iterator_nr
    , fminunc_nr_

    -- ** Quasi Newton methods
    , Iterator_bfgs
    , fminunc_bfgs_

    -- ** Multivariate line search
    , LineSearch

    , mkLineSearch
    , lineSearch_brent
    , lineSearch_gss

    -- *** Backtracking
    -- | Backtracking is much faster than univariate line search methods.
    -- It typically requires 1-3 function evaluations, whereas the univariate optimizations typically require 5-50.
    -- The downside is that backtracking is not guaranteed to converge to the minimum along the line search.
    -- In many cases, however, this is okay.
    -- We make up for the lack of convergence by being able to run many more iterations of the multivariate optimization.
    , Backtracking (..)
    , backtracking

    , wolfe
    , amijo
    , weakCurvature
    , strongCurvature

    -- * Test functions
    -- | See <https://en.wikipedia.org/wiki/Test_functions_for_optimization wikipedia> for more detail.
    --
    -- FIXME: Include graphs in documentation.
    --
    -- FIXME: Implement the rest of the functions.

    , sphere
    , ackley
    , beale
    , rosenbrock
    )
    where

import SubHask
import SubHask.Algebra.Vector
import SubHask.Category.Trans.Derivative

import HLearn.History
import HLearn.Optimization.Univariate

-------------------------------------------------------------------------------

type MultivariateMinimizer (n::Nat) cxt opt v
    = LineSearch cxt v
   -> StopCondition (opt v)
   -> v
   -> Diff n v (Scalar v)
   -> History cxt (opt v)

-------------------------------------------------------------------------------
-- generic transforms

-- FIXME: broken by new History
-- projectOrthant opt1 = do
--     mopt0 <- prevValueOfType opt1
--     return $ case mopt0 of
--         Nothing -> opt1
--         Just opt0 -> set x1 (VG.zipWith go (opt0^.x1) (opt1^.x1)) $ opt1
--     where
--         go a0 a1 = if (a1>=0 && a0>=0) || (a1<=0 && a0<=0)
--             then a1
--             else 0

-------------------------------------------------------------------------------
-- Conjugate gradient descent

data Iterator_cgd a = Iterator_cgd
    { _cgd_x1      :: !a
    , _cgd_fx1     :: !(Scalar a)
    , _cgd_f'x1    :: !a
    , _cgd_alpha   :: !(Scalar a)
    , _cgd_f'x0    :: !a
    , _cgd_s0      :: !a
    , _cgd_f       :: !(a -> Scalar a)
    }
    deriving (Typeable)

type instance Scalar (Iterator_cgd a) = Scalar a
type instance Logic (Iterator_cgd a) = Bool

instance (Eq (Scalar a), Eq a) => Eq_ (Iterator_cgd a) where
    a1==a2
        = (_cgd_x1    a1 == _cgd_x1    a2)
       && (_cgd_fx1   a1 == _cgd_fx1   a2)
       && (_cgd_f'x1  a1 == _cgd_f'x1  a2)
--        && (_cgd_alpha a1 == _cgd_alpha a2)
--        && (_cgd_f'x0  a1 == _cgd_f'x0  a2)
--        && (_cgd_s0    a1 == _cgd_s0    a2)

instance (Hilbert a, Show a, Show (Scalar a)) => Show (Iterator_cgd a) where
--     show cgd = "CGD; x="++show (_cgd_x1 cgd)++"; f'x="++show (_cgd_f'x1 cgd)++"; fx="++show (_cgd_fx1 cgd)
    show cgd = "CGD; |f'x|="++show (size $ _cgd_f'x1 cgd)++"; fx="++show (_cgd_fx1 cgd)

instance Has_x1 Iterator_cgd v where x1 = _cgd_x1
instance Has_fx1 Iterator_cgd v where fx1 = _cgd_fx1

---------------------------------------

-- | method for determining the conjugate direction;
-- See <https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient wikipedia> for more details.
type ConjugateMethod v = Module v => v -> v -> v -> Scalar v

{-# INLINABLE steepestDescent #-}
steepestDescent :: ConjugateMethod v
steepestDescent _ _ _ = 0

{-# INLINABLE fletcherReeves #-}
fletcherReeves :: Hilbert v => ConjugateMethod v
fletcherReeves f'x1 f'x0 _ = f'x1 <> f'x1 / f'x0 <> f'x0

{-# INLINABLE polakRibiere #-}
polakRibiere :: Hilbert v => ConjugateMethod v
polakRibiere f'x1 f'x0 _ = (f'x1 <> (f'x1 - f'x0)) / (f'x0 <> f'x0)

{-# INLINABLE hestenesStiefel #-}
hestenesStiefel :: Hilbert v => ConjugateMethod v
hestenesStiefel f'x1 f'x0 s0 = -(f'x1 <> (f'x1 - f'x0)) / (s0 <> (f'x1 - f'x0))

----------------------------------------

-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
{-# INLINEABLE fminunc_cgd_ #-}
fminunc_cgd_ ::
    ( Hilbert v
    , ClassicalLogic v
    ) => ConjugateMethod v -> MultivariateMinimizer 1 cxt Iterator_cgd v
fminunc_cgd_ conj lineSearch stop x0 f = {-# SCC fminunc_cgd #-}
    iterate (step_cgd lineSearch conj f) stop $ Iterator_cgd
        { _cgd_x1 = x0
        , _cgd_fx1 = f $ x0
        , _cgd_f'x1 = f' $ x0
        , _cgd_alpha = 1e-2
        , _cgd_f'x0 = 2 *. f' x0
        , _cgd_s0 = f' x0
        , _cgd_f = (f $)
        }
    where
        f' = (derivative f $)

step_cgd ::
    ( Hilbert v
    , ClassicalLogic v
    ) => LineSearch cxt v
      -> ConjugateMethod v
      -> C1 (v -> Scalar v)
      -> Iterator_cgd v
      -> History cxt (Iterator_cgd v)
step_cgd stepMethod conjMethod f (Iterator_cgd x1 fx1 f'x1 alpha1 f'x0 s0 _) = {-# SCC step_cgd #-} do
    let f' = (derivative f $)

    -- This test on beta ensures that conjugacy will be reset when it is lost.
    -- The formula is taken from equation 1.174 of the book "Nonlinear Programming".
    --
    -- FIXME:
    -- This test isn't needed in linear optimization, and the inner products are mildly expensive.
    -- It also makes CGD work only in a Hilbert space.
    -- Is the restriction worth it?
--     let beta = if abs (f'x1<>f'x0) > 0.2*(f'x0<>f'x0)
--             then 0
--             else max 0 $ conjMethod f'x1 f'x0 s0

    let beta = conjMethod f'x1 f'x0 s0

    let s1 = -f'x1 + beta *. s0

    alpha <- stepMethod (f $) f' x1 s1 alpha1

    let x = x1 + alpha *. s1

    return $ Iterator_cgd
        { _cgd_x1 = x
        , _cgd_fx1 = f $ x
        , _cgd_f'x1 = f' $ x
        , _cgd_alpha = alpha
        , _cgd_f'x0 = f'x1
        , _cgd_s0 = s1
        , _cgd_f = (f $)
        }

-------------------------------------------------------------------------------
-- The Newton-Raphson method (quadratic gradient descent)

data Iterator_nr v = Iterator_nr
    { _nr_x1      :: !v
    , _nr_fx1     :: !(Scalar v)
    , _nr_fx0     :: !(Scalar v)
    , _nr_f'x1    :: !v
    , _nr_alpha1  :: !(Scalar v)
    }
    deriving (Typeable)

instance Show (Iterator_nr v) where
    show _ = "Iterator_nr"

type instance Scalar (Iterator_nr a) = Scalar a

instance Has_x1 Iterator_nr a where x1 = _nr_x1
instance Has_fx1 Iterator_nr a where fx1 = _nr_fx1

----------------------------------------

{-# INLINEABLE fminunc_nr_ #-}
fminunc_nr_ ::
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => MultivariateMinimizer 2 cxt Iterator_nr v
fminunc_nr_ _ stop x0 f = iterate (step_nr f) stop $ Iterator_nr
    { _nr_x1 = x0
    , _nr_fx1 = f $ x0
    , _nr_fx0 = infinity
    , _nr_f'x1 = derivative f $ x0
    , _nr_alpha1 = 1
    }

step_nr :: forall v cxt.
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => C2 (v -> Scalar v)
      -> Iterator_nr v
      -> History cxt (Iterator_nr v)
step_nr f (Iterator_nr x0 fx0 _ f'x0 alpha0) = do
    let f'  = (derivative f $)
        f'' = ((derivative . derivative $ f) $)

    -- FIXME: We need regularization when the 2nd derivative is degenerate
    let dir = reciprocal (f'' x0) `mXv` f'x0

    -- FIXME: We should have the option to do line searches using any method
--     alpha <- do
--         let g :: v -> Scalar v
--             g y = f $ x0 + (y .* dir)
--
--         bracket <- LineMin.lineBracket g (alpha0/2) (alpha0*2)
--         brent <- LineMin.fminuncM_brent_ (return.g) bracket
--             ( maxIterations 100
-- --             + fx1grows
--             )
--         return $ LineMin._brent_x brent

    let alpha=1

    let x1 = x0 + alpha*.dir

    return $ Iterator_nr
        { _nr_x1 = x1
        , _nr_fx1 = f $ x1
        , _nr_fx0 = fx0
        , _nr_f'x1 = f' $ x1
        , _nr_alpha1 = 1 -- alpha
        }

-------------------------------------------------------------------------------
-- The BFGS quasi-newton method

-- | FIXME:
-- We currently build the full matrix, making things slower than they need to be.
-- Fixing this probably requires extending the linear algebra in subhask.
--
-- FIXME:
-- Also, the code converges much slower than I would expect for some reason.
data Iterator_bfgs v = Iterator_bfgs
    { _bfgs_x1      :: !v
    , _bfgs_fx1     :: !(Scalar v)
    , _bfgs_fx0     :: !(Scalar v)
    , _bfgs_f'x1    :: !v
    , _bfgs_f''x1   :: !((v><v))
    , _bfgs_alpha1  :: !(Scalar v)
    }
    deriving (Typeable)

type instance Scalar (Iterator_bfgs v) = Scalar v

instance (Show v, Show (Scalar v)) => Show (Iterator_bfgs v) where
    show i = "BFGS; x="++show (_bfgs_x1 i)++"; f'x="++show (_bfgs_f'x1 i)++"; fx="++show (_bfgs_fx1 i)

instance Has_x1 Iterator_bfgs a where x1 = _bfgs_x1
instance Has_fx1 Iterator_bfgs a where fx1 = _bfgs_fx1

{-# INLINEABLE fminunc_bfgs_ #-}
fminunc_bfgs_ ::
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => MultivariateMinimizer 1 cxt Iterator_bfgs v
fminunc_bfgs_ linesearch stop x0 f = iterate (step_bfgs f linesearch) stop $ Iterator_bfgs
    { _bfgs_x1 = x0
    , _bfgs_fx1 = f $ x0
    , _bfgs_fx0 = infinity
    , _bfgs_f'x1 = derivative f $ x0
    , _bfgs_f''x1 = 1
    , _bfgs_alpha1 = 1e-10
    }

step_bfgs :: forall v cxt.
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => C1 ( v -> Scalar v)
      -> LineSearch cxt v
      -> Iterator_bfgs v
      -> History cxt (Iterator_bfgs v)
step_bfgs f linesearch opt = do
    let f' = (derivative f $)

    let x0 = _bfgs_x1 opt
        fx0 = _bfgs_fx1 opt
        f'x0 = _bfgs_f'x1 opt
        f''x0 = _bfgs_f''x1 opt
        alpha0 = _bfgs_alpha1 opt

    let d = -f''x0 `mXv` f'x0
--         g  alpha = f  $ x0 + alpha *. d
--         g' alpha = f' $ x0 + alpha *. d

--     bracket <- lineBracket g (alpha0/2) (alpha0*2)
--     brent <- fminuncM_brent_ (return.g) bracket (maxIterations 200 || stop_brent 1e-6)
--     let alpha = x1 brent
    alpha <- linesearch (f $) f' x0 f'x0 alpha0

    let x1 = x0 - alpha *. d
        f'x1 = f' x1

    let p =   x1 -   x0
        q = f'x1 - f'x0

    let xsi = 1
        tao = q <> (f''x0 `mXv` q)
        v = p ./ (p<>q) - (f''x0 `mXv` q) ./ tao
        f''x1 = f''x0
             + (p >< p) ./ (p<>q)
             - (f''x0 * (q><q) * f''x0) ./ tao
             + (xsi*tao) *. (v><v)

--     let go a1 a0 = if (a1>=0 && a0 >=0) || (a1<=0&&a0<=0)
--             then a1
--             else a1
--         x1mod = VG.zipWith go x1 x0
    let x1mod = x1

    return $ Iterator_bfgs
        { _bfgs_x1 = x1mod
        , _bfgs_fx1 = f $ x1mod
        , _bfgs_fx0 = fx0
        , _bfgs_f'x1 = f' $ x1mod
        , _bfgs_f''x1 = f''x1
        , _bfgs_alpha1 = alpha
        }


-------------------------------------------------------------------------------
-- Line search

-- | Functions of this type determine how far to go in a particular direction.
-- These functions perform the same task as "UnivariateMinimizer",
-- but the arguments are explicitly multidimensional.
type LineSearch cxt v
    = (v -> Scalar v)       -- ^ f  = function
   -> (v -> v)              -- ^ f' = derivative of the function
   -> v                     -- ^ x0 = starting position
   -> v                     -- ^ f'(x0) = direction
   -> Scalar v              -- ^ initial guess at distance to minimum
   -> History cxt (Scalar v)

-- | Convert a "UnivariateMinimizer" into a "LineSearch".
{-# INLINEABLE mkLineSearch #-}
mkLineSearch ::
    ( Has_x1 opt (Scalar v)
    , VectorSpace v
    , OrdField (Scalar v)
    , cxt (LineBracket (Scalar v))
    , cxt (opt (Scalar v))
    , cxt (Scalar (opt (Scalar v)))
    ) => proxy opt                                      -- ^ required for type checking
      -> UnivariateMinimizer cxt opt (Scalar v)
      -> StopCondition (opt (Scalar v))                 -- ^ controls the number of iterations
      -> LineSearch cxt v
mkLineSearch _ fminuncM stops f _ x0 dir stepGuess = {-# SCC uni2multiLineSearch #-} do
    let g y = f $ x0 + dir.*y
    opt <- fminuncM stops stepGuess (return . g)
    return $ x1 opt

-- | Brent's method promoted to work on a one dimension subspace.
{-# INLINEABLE lineSearch_brent #-}
lineSearch_brent ::
    ( VectorSpace v
    , OrdField (Scalar v)
    , cxt (LineBracket (Scalar v))
    , cxt (Iterator_brent (Scalar v))
    ) => StopCondition (Iterator_brent (Scalar v))
      -> LineSearch cxt v
lineSearch_brent stops = mkLineSearch (Proxy::Proxy Iterator_brent) fminuncM_brent stops

-- | Golden section search promoted to work on a one dimension subspace.
{-# INLINEABLE lineSearch_gss #-}
lineSearch_gss ::
    ( VectorSpace v
    , OrdField (Scalar v)
    , cxt (LineBracket (Scalar v))
    , cxt (Iterator_gss (Scalar v))
    ) => StopCondition (Iterator_gss (Scalar v))
      -> LineSearch cxt v
lineSearch_gss stops = mkLineSearch (Proxy::Proxy Iterator_gss) fminuncM_gss stops

-------------------------------------------------------------------------------
-- backtracking

data Backtracking v = Backtracking
    { _bt_x     :: !(Scalar v)
    , _bt_fx    :: !(Scalar v)
    , _bt_f'x   :: !v

    , _init_dir :: !v
    , _init_f'x :: !v
    , _init_fx  :: !(Scalar v)
    , _init_x   :: !v
    }
    deriving (Typeable)

type instance Scalar (Backtracking v) = Scalar v

instance Show (Backtracking v) where
    show _ = "Backtracking"

instance Has_fx1 Backtracking v where
    fx1 = _bt_fx

-- | Backtracking linesearch is NOT guaranteed to converge.
-- It is frequently used as the linesearch for multidimensional problems.
-- In this case, the overall minimization problem can converge significantly
-- faster than if one of the safer methods is used.
{-# INLINABLE backtracking #-}
backtracking ::
    ( Hilbert v
    , cxt (Backtracking v)
    ) => StopCondition (Backtracking v)
      -> LineSearch cxt v
backtracking stop f f' x0 f'x0 stepGuess = {-# SCC backtracking #-} beginFunction "backtracking" $ do
    let g y = f $ x0 + y *. f'x0
    let grow=1.65

    fmap _bt_x $ iterate (step_backtracking 0.85 f f') stop $ Backtracking
        { _bt_x = (grow*stepGuess)
        , _bt_fx = g (grow*stepGuess)
        , _bt_f'x = grow *. (f' $ x0 + grow*stepGuess *. f'x0)
        , _init_dir = f'x0
        , _init_x = x0
        , _init_fx = f x0
        , _init_f'x = f'x0
        }

step_backtracking ::
    ( Module v
    ) => Scalar v
      -> (v -> Scalar v)
      -> (v -> v)
      -> Backtracking v
      -> History cxt (Backtracking v)
step_backtracking !tao !f !f' !bt = {-# SCC step_backtracking #-} do
    let x1 = tao * _bt_x bt
    return $ bt
        { _bt_x = x1
        , _bt_fx = g x1
        , _bt_f'x = g' x1
        }
    where
        g  alpha =          f  (_init_x bt + alpha *. _init_dir bt)
        g' alpha = alpha *. f' (_init_x bt + alpha *. _init_dir bt)

---------------------------------------
-- stop conditions

{-# INLINABLE wolfe #-}
wolfe :: Hilbert v => Scalar v -> Scalar v -> StopCondition (Backtracking v)
wolfe !c1 !c2 !bt0 !bt1 = {-# SCC wolfe #-} do
    a <- amijo c1 bt0 bt1
    b <- strongCurvature c2 bt0 bt1
    return $ a && b

{-# INLINABLE amijo #-}
amijo :: Hilbert v => Scalar v -> StopCondition (Backtracking v)
amijo !c1 _ !bt = {-# SCC amijo #-} return $
    _bt_fx bt <= _init_fx bt + c1 * (_bt_x bt) * ((_init_f'x bt) <> (_init_dir bt))

{-# INLINABLE weakCurvature #-}
weakCurvature :: Hilbert v => Scalar v -> StopCondition (Backtracking v)
weakCurvature !c2 _ !bt = {-# SCC weakCurvature #-} return $
    _init_dir bt <> _bt_f'x bt >= c2 * (_init_dir bt <> _init_f'x bt)

{-# INLINABLE strongCurvature #-}
strongCurvature :: Hilbert v => Scalar v -> StopCondition (Backtracking v)
strongCurvature !c2 _ !bt = {-# SCC strongCurvature #-} return $
    abs (_init_dir bt <> _bt_f'x bt) <= c2 * abs (_init_dir bt <> _init_f'x bt)

--------------------------------------------------------------------------------

-- | The simplest quadratic function
{-# INLINEABLE sphere #-}
sphere :: Hilbert v => C1 (v -> Scalar v)
sphere = unsafeProveC1 f f' -- f''
    where
        f   v = v<>v+3
        f'  v = 2*.v
--         f'' _ = 2

-- |
--
-- > ackley [0,0] == 0
--
-- ackley :: (IsScalar r, Field r) => SVector 2 r -> r
{-# INLINEABLE ackley #-}
ackley :: SVector 2 Double -> Double
ackley v = -20 * exp (-0.2 * sqrt $ 0.5 * (x*x + y*y)) - exp(0.5*cos(2*pi*x)+0.5*cos(2*pi*y)) + exp 1 + 20
    where
        x=v!0
        y=v!1

-- |
--
-- > beale [3,0.5] = 0
{-# INLINEABLE beale #-}
beale :: SVector 2 Double -> Double
beale v = (1.5-x+x*y)**2 + (2.25-x+x*y*y)**2 + (2.625-x+x*y*y*y)**2
    where
        x=v!0
        y=v!1

-- | The Rosenbrock function is hard to optimize because it has a narrow quadratically shaped valley.
--
-- See <https://en.wikipedia.org/wiki/Rosenbrock_function wikipedia> for more details.
--
-- FIXME:
-- The derivatives of this function are rather nasty.
-- We need to expand "SubHask.Category.Trans.Derivative" to handle these harder cases automatically.
{-# INLINEABLE rosenbrock #-}
rosenbrock :: (ValidElem v (Scalar v), ExpRing (Scalar v), FiniteModule v) => C1 (v -> Scalar v)
rosenbrock = unsafeProveC1 f f'
    where
        f v = sum [ 100*(v!(i+1) - (v!i)**2)**2 + (v!i - 1)**2 | i <- [0..dim v-2] ]

        f' v = imap go v
            where
                go i x = if i==dim v-1
                    then pt2
                    else if i== 0
                        then pt1
                        else pt1+pt2
                    where
                        pt1 = 400*x*x*x - 400*xp1*x + 2*x -2
                        pt2 = 200*x - 200*xm1*xm1
                        xp1 = v!(i+1)
                        xm1 = v!(i-1)

