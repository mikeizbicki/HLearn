{-# LANGUAGE DeriveDataTypeable,TemplateHaskell,DataKinds #-}
module HLearn.Optimization.GradientDescent
    ( conjugateGradientDescent
    , steepestDescent
    , conjugateGradientDescent_
    , ConjugateGradientDescent (..)
    , StepMethod (..)
    , ConjugateMethod (..)
    , traceConjugateGradientDescent
    , linsolvepsd
    )
    where

import Control.DeepSeq
import Control.Lens
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
-- import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Algebra.LinearAlgebra
-- import HLearn.Algebra.Structures.Monad
import HLearn.Optimization.Common
import HLearn.Optimization.Trace
import HLearn.Optimization.QuasiNewton hiding (__x1,__fx1,__f'x1,_x1,_fx1,_f'x1) 
import HLearn.Optimization.NewtonRaphson hiding (__x1,__fx1,__f'x1,_x1,_fx1,_f'x1) 
import HLearn.Optimization.LineMinimization as LineMin

-------------------------------------------------------------------------------
-- data types

data ConjugateGradientDescent a = ConjugateGradientDescent 
    { __x1      :: !(Tensor 1 a)
    , __fx1     :: !(Tensor 0 a)
    , __f'x1    :: !(Tensor 1 a)
    , __alpha   :: !(Tensor 0 a)
    , __f'x0    :: !(Tensor 1 a)
    , __s0      :: !(Tensor 1 a)
    }
    deriving (Typeable)
makeLenses ''ConjugateGradientDescent

instance ValidTensor1 v => Has_x1 ConjugateGradientDescent v where x1 = _x1
instance ValidTensor1 v => Has_fx1 ConjugateGradientDescent v where fx1 = _fx1
instance ValidTensor1 v => Has_f'x1 ConjugateGradientDescent v where f'x1 = _f'x1
instance ValidTensor1 v => Has_stepSize ConjugateGradientDescent v where stepSize = _alpha

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
-- conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch FletcherReeves f f'
conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch PolakRibiere f f'
-- conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch HestenesStiefel f f'
-- conjugateGradientDescent f f' = conjugateGradientDescent_ LineSearch None f f'
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
conjugateGradientDescent_ searchMethod conjugateMethod f f' x0 = do
    optimize
        (step_conjugateGradientDescent searchMethod conjugateMethod f f')
        $ ConjugateGradientDescent
            { __x1 = x0
            , __fx1 = f x0
    --         , __fx1 = infinity -- f x0
            , __f'x1 = f' x0
            , __alpha = 1e-2
            , __f'x0 = 2 .* f' x0
            , __s0 = f' x0
            }

-- | performs a single iteration of the conjugate gradient descent algorithm
step_conjugateGradientDescent :: 
    ( ValidTensor1 v
    , IsScalar (Scalar v)
    , v ~ Tensor 1 v
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    ) => StepMethod (Scalar v)
      -> ConjugateMethod
      -> (v -> Scalar v)
      -> (v -> v)
      -> ConjugateGradientDescent v
      -> History (ConjugateGradientDescent v)
step_conjugateGradientDescent stepMethod conjMethod f f' (ConjugateGradientDescent x1 fx1 f'x1 alpha1 f'x0 s0) = do
    -- | this formula is taken from equation 1.174 of the book "Nonlinear Programming"
    let gamma = 0.2
        lostConjugacy = abs (inner f'x1 f'x0) > gamma * squaredInnerProductNorm f'x0 

    let rawbeta = case conjMethod of
            None -> 0
            FletcherReeves -> inner f'x1 f'x1 / inner f'x0 f'x0
            PolakRibiere -> (inner f'x1 (f'x1 <> inverse f'x0)) / (inner f'x0 f'x0)
            HestenesStiefel -> -(inner f'x1 (f'x1 <> inverse f'x0)) / (inner s0 (f'x1 <> inverse f'x0))

    -- | this test ensures that conjugacy will be reset when it is lost 
    let beta = if lostConjugacy
            then 0
            else max 0 $ rawbeta

--     let s1 = (inverse f'x1) <> beta .* f'x1
    let s1 = (inverse f'x1) <> beta .* s0

    let g y = f $ x1 <> y .* s1

    alpha <- case stepMethod of
        StepSize x -> return x
        LineSearch -> do
            let grow=2.1
            fmap _bt_x $ backtracking (0.5) f f' 
                (Backtracking
                    { _bt_x = (grow*alpha1)
                    , _bt_fx = g (grow*alpha1)
                    , _bt_f'x = grow .* (f' $ x1 <> grow*alpha1 .* s1)
                    , _init_dir = s1
                    , _init_x = x1
                    , _init_fx = fx1
                    , _init_f'x = f'x1
                    })
--                 [stop_wolfe 1e-4 0.1]
                [stop_amijo 1e-4]
            
--         LineSearch -> do
--             bracket <- LineMin.lineBracket g (alpha1/2) (alpha1*2)
--             brent <- LineMin.brent g bracket 
--                 [ LineMin.brentTollerance 1e-12
--                 , maxIterations 100
-- --                 , lowerBound fx1
--                 ]
--             return $ LineMin._x brent

    let x = x1 <> alpha .* s1

    return $ ConjugateGradientDescent
        { __x1 = x
        , __fx1 = f x
        , __f'x1 = f' x
        , __alpha = alpha
        , __f'x0 = f'x1
        , __s0 = s1
        }

-------------------------------------------------------------------------------
-- trace

traceConjugateGradientDescent :: Event -> [String]
traceConjugateGradientDescent = traceFunk (undefined :: ConjugateGradientDescent (Vector Double))
-- traceConjugateGradientDescent _ = traceFunk (undefined :: ConjugateGradientDescent dp)

-------------------------------------------------------------------------------
-- tests

numdim :: Double
numdim = 2

mat :: LA.Matrix Double
mat = (2 LA.>< 2) [1,1.5,1.5,2]
-- mat = eye $ floor numdim
-- mat = foldl1 (<>) [go i | i <- [1..numdim] ]
--     where
--         go i = outerProductV x x
--             where
--                 x = VG.fromList $ map ( (*i) . indicator . (==i)) $ [1..numdim]
--                 x = VG.map sqrt $ VG.fromList [i..numdim+i-1]

vec :: Vector Double
vec = VG.fromList [i | i <- [1..numdim]]
-- vec = VG.fromList [i**2 | i <- [1..numdim]]

linsolvepsd :: forall v. 
    ( ValidTensor_ v
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    , Typeable (Tensor 1 v)
    ) => Tensor 2 v
      -> v
      -> History (Tensor 1 v)
linsolvepsd a b' = fmap (^.x1) $ conjugateGradientDescent_ LineSearch FletcherReeves f f' zero 
-- linsolvesd a b' = fmap (^.x1) $ conjugateGradientDescent_ LineSearch None f f' zero 
    [ multiplicativeTollerance 1e-6
    , maxIterations 20
    ]
    where
        b = mkTensor b'
        zero = b <> inverse b -- mempty -- VG.map (const 0) b
        f   x = (1/2) .* (inner x (mul a' x)) <> inverse (inner x b)
        f'  x = mul a' x <> inverse b
        f'' x = a'

        a' = a -- <> eye (rows a)

test :: IO (Vector Double)
test = do
    let (res,hist) = unsafeRunHistory $ linsolvepsd mat vec
    printHistory 
        [ traceType (Proxy::Proxy (ConjugateGradientDescent (Vector Double))) 
            [ shortName, numItr, show_fx1, show_f'x1, showSeconds
            ]
        , traceType (Proxy::Proxy (Backtracking (Vector Double)))
            [ shortName, numItr, show_fx1, showSeconds ] 
        ]
--         [ traceConjugateGradientDescent
--         , traceBFGS
--         , traceNewtonRaphson
--         , traceBracket
--         , traceBrent
--         , traceBacktracking (undefined :: Vector Double)
--         ]
        hist
    print "minimum"
    print $ mul mat res <> inverse vec
    return res
