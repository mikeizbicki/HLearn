{-# LANGUAGE DataKinds #-}

module HLearn.Optimization.NewtonRaphson
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
-- import qualified Numeric.LinearAlgebra as LA
import qualified HLearn.Algebra.LinearAlgebra as LA
import HLearn.Algebra.LinearAlgebra (ValidTensor(..))
import HLearn.Optimization.Common
import qualified HLearn.Optimization.LineMinimization as LineMin


data NewtonRaphson a = NewtonRaphson
    { _x1 :: !(Tensor 1 a)
    , _fx1 :: !(Tensor 0 a)
    , _f'x1 :: !(Tensor 1 a)
    }
    deriving (Typeable)

instance (a~Tensor 1 a) => Has_x1 NewtonRaphson a where x1 = _x1
instance (ValidTensor a) => Has_fx1 NewtonRaphson a where fx1 = _fx1
instance (a~Tensor 1 a) => Has_f'x1 NewtonRaphson a where f'x1 = _f'x1

deriving instance (Typeable Matrix)

newtonRaphson f f' f'' x0 = do
    let nr0 = NewtonRaphson x0 (f x0) (f' x0)
    nr1 <- step_newtonRaphson f f' f'' nr0
    optimize
        ( _stop_itr 100
        <> _stop_tolerance _fx1 1e-6
        <> [\opt -> _fx1 (curValue opt) < -1e20]
        )
        (step_newtonRaphson f f' f'')
        (initTrace nr1 nr0)

step_newtonRaphson :: 
    ( ValidTensor v
    , Tensor 2 v ~ LA.Matrix (Scalar v)
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    , OptMonad m
    ) => (Tensor 1 v -> Tensor 0 v)
      -> (Tensor 1 v -> Tensor 1 v)
      -> (Tensor 1 v -> Tensor 2 v)
      -> NewtonRaphson v
      -> m (NewtonRaphson v)
step_newtonRaphson f f' f'' opt = do
    let x = _x1 opt
        f'x = _f'x1 opt
        f''x = f'' x
        dir = (-1) .* (LA.inv f''x `LA.mul` f'x)
    
    let xtmp = x <> dir
        fxtmp = f xtmp

--     addMessage "no line minimize"
    (x',fx') <- {-trace ("fxtmp="++show fxtmp++"; _fx1 opt="++show (_fx1 opt)) $-} if fxtmp < _fx1 opt
        then do
--             addMessage "no line minimize"
            return (xtmp,fxtmp)
        else do
--             addMessage "line minimizing"
            let g y = f $ x <> y .* dir
            alpha <- do
                bracket <- LineMin.lineBracket g (-1e-6) (-1)
                brent <- LineMin.brent g $ curValue bracket
                return $ LineMin._x $ curValue brent
            let x' = x <> alpha .* dir
            return (x', f x')

    report $ NewtonRaphson
        { _x1 = x'
        , _fx1 = fx'
        , _f'x1 = f' x'
        }
