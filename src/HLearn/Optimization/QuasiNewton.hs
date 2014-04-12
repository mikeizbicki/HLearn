{-# LANGUAGE TemplateHaskell,DataKinds #-}
module HLearn.Optimization.QuasiNewton
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Control.Lens
import Data.List
import Data.List.Extras
import Data.Typeable
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro
import Numeric.LinearAlgebra hiding ((<>))

import HLearn.Algebra
import HLearn.Algebra.LinearAlgebra as LA
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization as LineMin


data BFGS a = BFGS
    { __x1      :: !(Tensor 1 a)
    , __fx1     :: !(Tensor 0 a)
    , __fx0     :: !(Tensor 0 a)
    , __f'x1    :: !(Tensor 1 a)
    , __f''x1   :: !(Tensor 2 a)
    , __alpha1  :: !(Tensor 0 a)
    }
--     { __x1 :: !a
--     , __fx1 :: !(Scalar a)
--     , __fx0 :: !(Scalar a)
--     , __f'x1 :: !a
--     , __f''x1 :: !(LA.Matrix (Scalar a))
--     , __alpha1 :: !(Scalar a)
--     }
    deriving (Typeable)
makeLenses ''BFGS

-- deriving instance (Show a, Show (Scalar a), Show (LA.Matrix (Scalar a))) => Show (BFGS a)

instance (ValidTensor a) => Has_x1 BFGS a where x1 = _x1
instance (ValidTensor a) => Has_fx1 BFGS a where fx1 = _fx1
instance (ValidTensor a) => Has_fx0 BFGS a where fx0 = _fx0
instance (ValidTensor a) => Has_f'x1 BFGS a where f'x1 = _f'x1
instance (ValidTensor a) => Has_stepSize BFGS a where stepSize = _alpha1

-- instance Has_x1 BFGS a where x1 = _x1
-- instance Has_fx1 BFGS a where fx1 = _fx1
-- instance Has_fx0 BFGS a where fx0 = _fx0
-- instance Has_f'x1 BFGS a where f'x1 = _f'x1
-- instance Has_stepSize BFGS a where stepSize = _alpha1

quasiNewton' f f' f'' x0 = optimize
    (step_quasiNewton f f')
    $ BFGS 
        { __x1 = x0 
        , __fx1 = f x0
        , __fx0 = infinity
        , __f'x1 = f' x0
        , __f''x1 = f'' x0
        , __alpha1 = 1e-2
        }

quasiNewton f f' x0 = optimize
    ( step_quasiNewton f f' )
    ( BFGS 
        { __x1 = x0 
        , __fx1 = f x0
        , __fx0 = infinity
        , __f'x1 = f' x0
        , __f''x1 = LA.eye (VG.length x0)
        , __alpha1 = 1e-10
        }
    )

step_quasiNewton :: 
    ( vec ~ LA.Vector r
    , mat ~ LA.Matrix r
    , r ~ Scalar r
    , VectorSpace r
    , Field r
    , Ord r
    , Typeable r
    , Show r
    , IsScalar r
    ) => (vec -> r)
      -> (vec -> vec)
      -> BFGS vec
      -> History (BFGS vec)
step_quasiNewton f f' opt = do
    let x0 = opt^.x1
        fx0 = opt^.fx1
        f'x0 = opt^.f'x1
        f''x0 = opt^._f''x1
        alpha0 = opt^.stepSize
        d = inverse $ f''x0 `LA.matProduct` f'x0
        g alpha = f $ x0 <> alpha .* d

--     alpha <- fmap _bt_x $ backtracking (0.75) f f' 
--         (Backtracking
--             { _bt_x = 1 
--             , _bt_fx = g 1
--             , _bt_f'x = f' $ x0 <> d
--             , _init_dir = d
--             , _init_x = x0
--             , _init_fx = fx0
--             , _init_f'x = f'x0
--             })
--         [ stop_wolfe 1e-4 0.9
--         , maxIterations 100
--         ]

    bracket <- LineMin.lineBracket g (alpha0/2) (alpha0*2)
    brent <- LineMin.brent g bracket [maxIterations 200,LineMin.brentTollerance 1e-6]
    let alpha = LineMin._x  brent

    let x1 = x0 <> alpha .* d
        f'x1 = f' x1

    let p=x1 <> inverse x0
        q=f'x1 <> inverse f'x0
        
    let xsi = 1
        tao = inner q $ f''x0 `LA.matProduct` q
        v = p /. (inner p q) <> inverse (f''x0 `LA.matProduct` q) /. tao
        f''x1 = f''x0 
             <> (LA.v2m p `LA.matProduct` LA.v2m' p) /. (inner p q)
             <> inverse (f''x0 `LA.matProduct` LA.v2m q 
                               `LA.matProduct` LA.v2m' q
                               `LA.matProduct` f''x0) /. tao
             <> (xsi*tao) .* (LA.v2m v `LA.matProduct` LA.v2m' v)

    let go a1 a0 = if (a1>=0 && a0 >=0) || (a1<=0&&a0<=0)
            then a1
            else a1
        x1mod = VG.zipWith go x1 x0

    return $ BFGS
        { __x1 = x1mod
        , __fx1 = f x1mod
        , __fx0 = opt^.fx1
        , __f'x1 = f' x1mod
        , __f''x1 = f''x1
        , __alpha1 = alpha
        }
--     report $ BFGS
--         { _x1 = x1
--         , _fx1 = f x1
--         , _fx0 = fx1 opt
--         , _f'x1 = f' x1
--         , _f''x1 = f''x1
--         , _alpha1 = alpha
--         }
