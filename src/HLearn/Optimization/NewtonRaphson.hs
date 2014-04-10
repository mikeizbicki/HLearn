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
import HLearn.Algebra.LinearAlgebra as LA
import HLearn.Optimization.Common
import qualified HLearn.Optimization.LineMinimization as LineMin


data NewtonRaphson a = NewtonRaphson
    { _x1 :: !(Tensor 1 a)
    , _fx1 :: !(Tensor 0 a)
    , _fx0 :: !(Tensor 0 a)
    , _f'x1 :: !(Tensor 1 a)
    , _alpha1 :: !(Tensor 0 a)
    }
    deriving (Typeable)

deriving instance (Show (Tensor 0 a), Show (Tensor 1 a)) => Show (NewtonRaphson a)

instance (a~Tensor 1 a) => Has_x1 NewtonRaphson a where x1 = _x1
instance (ValidTensor a) => Has_fx1 NewtonRaphson a where fx1 = _fx1
instance (ValidTensor a) => Has_fx0 NewtonRaphson a where fx0 = _fx0
instance (a~Tensor 1 a) => Has_f'x1 NewtonRaphson a where f'x1 = _f'x1
instance (ValidTensor a) => Has_stepSize NewtonRaphson a where stepSize = _alpha1

deriving instance (Typeable Matrix)

newtonRaphson f f' f'' x0 = optimize
--     ( _stop_itr 100
--     <> _stop_tolerance _fx1 1e-6
--     <> [\opt -> _fx1 (curValue opt) < -1e20]
--     )
    (step_newtonRaphson f f' f'')
    $ NewtonRaphson
        { _x1 = x0 
        , _fx1 = f x0
        , _fx0 = infinity
        , _f'x1 = f' x0
        , _alpha1 = 1
        }

projectOrthant opt1 = do
    mopt0 <- prevValueOfType opt1
    return $ case mopt0 of
        Nothing -> opt1
        Just opt0 -> opt1 { _x1 = VG.zipWith go (x1 opt0) (x1 opt1) }
    where
        go a0 a1 = if (a1>=0 && a0>=0) || (a1<=0 && a0<=0)
            then a1
            else 0

step_newtonRaphson :: 
    ( ValidTensor v
    , Tensor 2 v ~ LA.Matrix (Scalar v)
    , Tensor 1 v ~ LA.Vector (Scalar v)
    , v ~ LA.Vector (Scalar v)
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    ) => (Tensor 1 v -> Tensor 0 v)
      -> (Tensor 1 v -> Tensor 1 v)
      -> (Tensor 1 v -> Tensor 2 v)
      -> NewtonRaphson v
      -> History (NewtonRaphson v)
step_newtonRaphson f f' f'' opt = do
    let x0 = _x1 opt
        fx0 = _fx1 opt
        f'x0 = _f'x1 opt
        f''x0 = f'' x0 
        alpha0 = _alpha1 opt
    
    let reg=1
        dir = (-1) .* (LA.inv (f''x0 <> reg .* LA.eye (LA.rows f''x0)) `LA.mul` f'x0)

    alpha <- do
        let g y = f $ x0 <> y .* dir

        bracket <- LineMin.lineBracket g (alpha0/2) (alpha0*2)
        brent <- LineMin.brent g bracket
            [ lowerBound fx0
            , maxIterations 100
            ] 
        return $ LineMin._x brent

    let x1 = x0 <> alpha .* dir

--     return $ NewtonRaphson
    projectOrthant $ NewtonRaphson
        { _x1 = x1
        , _fx1 = f x1
        , _fx0 = fx1 opt
        , _f'x1 = f' x1
        , _alpha1 = alpha
        }
