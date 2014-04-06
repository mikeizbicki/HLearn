module HLearn.Optimization.QuasiNewton
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
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
import qualified HLearn.Optimization.LineMinimization as LineMin


data BFGS a = BFGS
    { _x1 :: !a
    , _fx1 :: !(Scalar a)
    , _fx0 :: !(Scalar a)
    , _f'x1 :: !a
    , _f''x1 :: !(LA.Matrix (Scalar a))
    , _alpha1 :: !(Scalar a)
    }
    deriving (Typeable)

deriving instance (Show a, Show (Scalar a), Show (LA.Matrix (Scalar a))) => Show (BFGS a)

instance Has_x1 BFGS a where x1 = _x1
instance Has_fx1 BFGS a where fx1 = _fx1
instance Has_fx0 BFGS a where fx0 = _fx0
instance Has_f'x1 BFGS a where f'x1 = _f'x1
instance Has_stepSize BFGS a where stepSize = _alpha1

quasiNewton' f f' f'' x0 = optimize
    (step_quasiNewton f f')
    $ BFGS 
        { _x1 = x0 
        , _fx1 = f x0
        , _fx0 = infinity
        , _f'x1 = f' x0
        , _f''x1 = f'' x0
        , _alpha1 = 1e-2
        }

quasiNewton f f' x0 = optimize
    ( step_quasiNewton f f' )
    ( BFGS 
        { _x1 = x0 
        , _fx1 = f x0
        , _fx0 = infinity
        , _f'x1 = f' x0
        , _f''x1 = LA.eye (VG.length x0)
        , _alpha1 = 1e-2
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
    ) => (vec -> r)
      -> (vec -> vec)
      -> BFGS vec
      -> History (BFGS vec)
step_quasiNewton f f' opt = do
    let x0 = _x1 opt
        f'x0 = _f'x1 opt
        f''x0 = _f''x1 opt
        alpha0 = _alpha1 opt
        d = inverse $ f''x0 `LA.matProduct` f'x0
        g alpha = f $ x0 <> alpha .* d

    bracket <- LineMin.lineBracket g (alpha0/2) (alpha0*2)
    brent <- LineMin.brent g bracket [LineMin.brentTollerance 1e-6]
    let alpha = LineMin._x  brent
        x1 = x0 <> alpha .* d
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

    report $ BFGS
        { _x1 = x1
        , _fx1 = f x1
        , _fx0 = fx1 opt
        , _f'x1 = f' x1
        , _f''x1 = f''x1
        , _alpha1 = alpha
        }
