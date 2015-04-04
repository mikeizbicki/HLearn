-- | A great introduction is: "An introduction to the conjugate gradient method without the pain" by Jonathan Richard Shewchuk
--
-- NOTE: Does this paper has a mistake in the calculation of the optimal step size; it should be multiplied by 1/2?
module HLearn.Optimization.GradientDescent
    ( fminunc_cgd
    , fminunc_cgd_

    -- ** conjugate methods
    , ConjugateMethod
    , steepestDescent
    , fletcherReeves
    , polakRibiere
    , hestenesStiefel

    -- ** Internal use only
    , Iterator_cgd (..)
--     , linsolvepsd
    )
    where

import SubHask

import HLearn.History
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate


-------------------------------------------------------------------------------

import SubHask.Compatibility.Vector
import qualified Data.Vector.Unboxed as VU

f :: Hilbert v => v -> v -> Scalar v
f  x y = (x-y)<>(x-y)

f' :: Hilbert v => v -> v -> v
f' x y = 2*.(x-y)

u = VU.fromList [1,2,3,4,5,6,7]     :: VU.Vector Double
v = VU.fromList [2,3,1,0,-2,-3,-1]  :: VU.Vector Double
w = VU.fromList [1,1]  :: VU.Vector Double

type instance Elem (VU.Vector a) = a
type instance Index (VU.Vector a) = Int
-- instance (Complemented (Logic a), VU.Unbox a) => IxContainer (VU.Vector a) where
instance IxContainer (VU.Vector Double) where
    (!) = (VU.!)

beale :: (Elem v~Scalar v, Index v~Int, IxContainer v, Hilbert v) => v -> Scalar v
beale v = (1.5-x+x*y)**2 + (2.25 - x + x*y**2)**2 + (2.625 - x + x*y**3)**2
    where
        x=v!0
        y=v!1

-- beale' :: (Elem v~Scalar v, Index v~Int, IxContainer v, Hilbert v) => v -> Scalar v
beale' :: VU.Vector Double -> VU.Vector Double
beale' v = VU.fromList
    [ 2*(-1+y)*(1.5-x+x*y)**2 + 2*(-1+y**2)*(2.25 - x + x*y**2)**2 + 2*(-1+y**3)*(2.625 - x + x*y**3)**2
    , 2*x*(1.5-x+x*y)**2 + 2*x*2*y*(2.25 - x + x*y**2)**2 + 2*x*3*y*y*(2.625 - x + x*y**3)**2
    ]
    where
        x=v!0
        y=v!1

-------------------------------------------------------------------------------
-- data types

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

-------------------------------------------------------------------------------
-- functions

-- | Conjugate gradient descent with reasonable default parameters
fminunc_cgd ::
    ( Hilbert v
    , BoundedField (Scalar v)
    , Optimizable v
    , Optimizable (Scalar v)
    ) => (v -> Scalar v)
      -> (v -> v)
      -> v
      -> History (Iterator_cgd v)
fminunc_cgd f f' x0 =
    fminunc_cgd_
        ( lineSearchBrent ( stop_brent 1e-12 || maxIterations 20 ))
        polakRibiere
        f
        f'
        x0
        ( maxIterations 20 || mulTolerance 1e-6 )


-- | A generic method for conjugate gradient descent that gives you more control over the optimization parameters
fminunc_cgd_ ::
    ( VectorSpace v
    , ClassicalLogic v
    , Optimizable v
    ) => MultivariateLineSearch v
      -> ConjugateMethod v
      -> (v -> Scalar v)
      -> (v -> v)
      -> v
      -> StopCondition_ (Iterator_cgd v)
      -> History (Iterator_cgd v)
fminunc_cgd_ searchMethod conjugateMethod f f' x0 = {-# SCC fminunc_cgd #-} iterate
    (step_cgd searchMethod conjugateMethod f f')
    $ Iterator_cgd
        { _cgd_x1 = x0
        , _cgd_fx1 = f x0
        , _cgd_f'x1 = f' x0
        , _cgd_alpha = 1e-2
        , _cgd_f'x0 = 2 *. f' x0
        , _cgd_s0 = f' x0
        , _cgd_f = f
        }

-- | performs a single iteration of the conjugate gradient descent algorithm
step_cgd ::
    ( VectorSpace v
    , ClassicalLogic v
    ) => MultivariateLineSearch v
      -> ConjugateMethod v
      -> (v -> Scalar v)
      -> (v -> v)
      -> Iterator_cgd v
      -> History (Iterator_cgd v)
step_cgd stepMethod conjMethod f f' (Iterator_cgd x1 fx1 f'x1 alpha1 f'x0 s0 _) = {-# SCC step_cgd #-} do
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

    return $ Iterator_cgd
        { _cgd_x1 = x
        , _cgd_fx1 = f x
        , _cgd_f'x1 = f' x
        , _cgd_alpha = alpha
        , _cgd_f'x0 = f'x1
        , _cgd_s0 = s1
        , _cgd_f = f
        }

