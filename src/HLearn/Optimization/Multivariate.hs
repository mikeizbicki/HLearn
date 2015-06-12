module HLearn.Optimization.Multivariate
    (

    -- * Multivariate optimization methods

    -- ** Conjugate gradient descent
    -- | A great introduction is:
    -- "An introduction to the conjugate gradient method without the pain" by Jonathan Richard Shewchuk
    --
    -- NOTE: Does this paper has a mistake in the calculation of the optimal step size;
    -- it should be multiplied by 1/2?
      fminunc_cgd
    , fminunc_cgd_
    , Iterator_cgd (..)

    -- *** Conjugation methods
    , ConjugateMethod
    , steepestDescent
    , fletcherReeves
    , polakRibiere
    , hestenesStiefel

    -- ** Newton-Raphson (quadratic gradient descent)
    , fminunc_nr_
    , Iterator_nr (..)

    -- ** Quasi Newton methods
    , fminunc_bfgs_
    , Iterator_bfgs (..)

    -- * Multivariate line search
    , MultivariateLineSearch

    -- ** Univariate wrappers
    , multivariateBrent

    -- ** Unsafe line minimization
    , Backtracking (..)
    , backtracking

    , wolfe
    , amijo
    , weakCurvature
    , strongCurvature
    )
    where

import SubHask
import SubHask.Category.Trans.Derivative

import HLearn.History
import HLearn.Optimization.Univariate

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

instance Show (Iterator_cgd a) where
    show _ = "CGD"

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

-- | Conjugate gradient descent with reasonable default parameters
fminunc_cgd ::
    ( Hilbert v
    , BoundedField (Scalar v)
    , cxt (LineBracket (Scalar v))
    , cxt (Iterator_brent (Scalar v))
    ) => v
      -> C1 (v -> Scalar v)
      -> History cxt (Iterator_cgd v)
fminunc_cgd x0 f =
    fminunc_cgd_
        ( multivariateBrent ( stop_brent 1e-12 || maxIterations 20 ))
        polakRibiere
        x0
        f
        ( maxIterations 20 || mulTolerance 1e-6 )


-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
fminunc_cgd_ ::
    ( VectorSpace v
    , ClassicalLogic v
    ) => MultivariateLineSearch cxt v
      -> ConjugateMethod v
      -> v
      -> C1 (v -> Scalar v)
      -> StopCondition_ cxt (Iterator_cgd v)
      -> History cxt (Iterator_cgd v)
fminunc_cgd_ searchMethod conjugateMethod x0 f = {-# SCC fminunc_cgd #-} iterate
    (step_cgd searchMethod conjugateMethod f)
    $ Iterator_cgd
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

-- | performs a single iteration of the conjugate gradient descent algorithm
step_cgd ::
    ( VectorSpace v
    , ClassicalLogic v
    ) => MultivariateLineSearch cxt v
      -> ConjugateMethod v
      -> C1 (v -> Scalar v)
      -> Iterator_cgd v
      -> History cxt (Iterator_cgd v)
step_cgd stepMethod conjMethod f (Iterator_cgd x1 fx1 f'x1 alpha1 f'x0 s0 _) = {-# SCC step_cgd #-} do
    let f' = (derivative f $)

    -- this test ensures that conjugacy will be reset when it is lost; the
    -- formula is taken from equation 1.174 of the book "Nonlinear Programming"

--     let beta = if abs (f'x1 <> f'x0) > 0.2 * squaredInnerProductNorm f'x0
--             then 0
--             else max 0 $ conjMethod f'x1 f'x0 s0

    let beta = conjMethod f'x1 f'x0 s0

    let s1 = - f'x1 + beta *. f'x1
--     let s1 = - f'x1 + beta *. s0

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

fminunc_nr_ ::
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => v
      -> C2 (v -> Scalar v)
      -> StopCondition_ cxt (Iterator_nr v)
      -> History cxt (Iterator_nr v)
fminunc_nr_ x0 f = iterate
    (step_nr f)
    $ Iterator_nr
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

instance Show (Iterator_bfgs v) where
    show _ = "BFGS"

instance Has_x1 Iterator_bfgs a where x1 = _bfgs_x1
instance Has_fx1 Iterator_bfgs a where fx1 = _bfgs_fx1

fminunc_bfgs_ ::
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => v
      -> C1 ( v -> Scalar v)
      -> MultivariateLineSearch cxt v
      -> StopCondition_ cxt (Iterator_bfgs v)
      -> History cxt (Iterator_bfgs v)
fminunc_bfgs_ x0 f linesearch = iterate
    ( step_bfgs f linesearch )
    ( Iterator_bfgs
        { _bfgs_x1 = x0
        , _bfgs_fx1 = f $ x0
        , _bfgs_fx0 = infinity
        , _bfgs_f'x1 = derivative f $ x0
        , _bfgs_f''x1 = 1
        , _bfgs_alpha1 = 1e-10
        }
    )

step_bfgs :: forall v cxt.
    ( Hilbert v
    , BoundedField (Scalar v)
    ) => C1 ( v -> Scalar v)
      -> MultivariateLineSearch cxt v
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

    let x1 = x0 + alpha *. d
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

-- | Backtracking linesearch is NOT guaranteed to converge.
-- It is frequently used as the linesearch for multidimensional problems.
-- In this case, the overall minimization problem can converge significantly
-- faster than if one of the safer methods is used.
backtracking ::
    ( Hilbert v
    , cxt (Backtracking v)
    ) => StopCondition_ cxt (Backtracking v)
      -> MultivariateLineSearch cxt v
backtracking stops f f' x0 f'x0 stepGuess = {-# SCC backtracking #-} do
    let g y = {-# SCC backtracking_g #-} f $ x0 + y *. f'x0
    let grow=2.1

    fmap _bt_x $ iterate (step_backtracking 0.5 f f')
        (Backtracking
            { _bt_x = (grow*stepGuess)
            , _bt_fx = g (grow*stepGuess)
            , _bt_f'x = grow *. (f' $ x0 + grow*stepGuess *. f'x0)
            , _init_dir = f'x0
            , _init_x = x0
            , _init_fx = f x0
            , _init_f'x = f'x0
            })
        stops

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
        g alpha = f $ _init_x bt + alpha *. _init_dir bt
        g' alpha = alpha *. f' (_init_x bt + alpha *. _init_dir bt)

---------------------------------------
-- stop conditions

{-# INLINABLE wolfe #-}
wolfe :: Hilbert v => Scalar v -> Scalar v -> StopCondition_ cxt (Backtracking v)
wolfe !c1 !c2 !bt0 !bt1 = {-# SCC wolfe #-} do
    a <- amijo c1 bt0 bt1
    b <- strongCurvature c2 bt0 bt1
    return $ a && b

{-# INLINABLE amijo #-}
amijo :: Hilbert v => Scalar v -> StopCondition_ cxt (Backtracking v)
amijo !c1 _ !bt = {-# SCC amijo #-} return $
    _bt_fx bt <= _init_fx bt + c1 * (_bt_x bt) * ((_init_f'x bt) <> (_init_dir bt))

{-# INLINABLE weakCurvature #-}
weakCurvature :: Hilbert v => Scalar v -> StopCondition_ cxt (Backtracking v)
weakCurvature !c2 _ !bt = {-# SCC weakCurvature #-} return $
    _init_dir bt <> _bt_f'x bt >= c2 * (_init_dir bt <> _init_f'x bt)

{-# INLINABLE strongCurvature #-}
strongCurvature :: Hilbert v => Scalar v -> StopCondition_ cxt (Backtracking v)
strongCurvature !c2 _ !bt = {-# SCC strongCurvature #-} return $
    abs (_init_dir bt <> _bt_f'x bt) <= c2 * abs (_init_dir bt <> _init_f'x bt)


-------------------------------------------------------------------------------

-- | determine how far to go in a particular direction
type MultivariateLineSearch cxt v
    = (v -> Scalar v)       -- ^ f  = function
    -> (v -> v)             -- ^ f' = derivative of the function
    -> v                    -- ^ x0 = starting position
    -> v                    -- ^ f'(x0) = direction
    -> Scalar v             -- ^ initial guess at distance to minimum
    -> History cxt (Scalar v)

multivariateBrent ::
    ( Hilbert v
    , cxt (LineBracket (Scalar v))
    , cxt (Iterator_brent (Scalar v))
    ) => StopCondition_ cxt (Iterator_brent (Scalar v))
      -> MultivariateLineSearch cxt v
multivariateBrent !stops !f _ !x0 !f'x0 !stepGuess = {-# SCC multivariateBrent #-} do
    let g y = f $ x0 + y *. f'x0
    bracket <- lineBracket g (stepGuess/2) (stepGuess*2)
    brent <- fminuncM_brent_ (return . g) bracket stops
    return $ _brent_x brent
