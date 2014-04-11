{-# LANGUAGE DataKinds, TemplateHaskell #-}

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
import Control.Lens

import HLearn.Algebra
-- import qualified Numeric.LinearAlgebra as LA
import HLearn.Algebra.LinearAlgebra as LA
import HLearn.Optimization.Common
import qualified HLearn.Optimization.LineMinimization as LineMin


data NewtonRaphson a = NewtonRaphson
    { __x1 :: !(Tensor 1 a)
    , __fx1 :: !(Tensor 0 a)
    , __fx0 :: !(Tensor 0 a)
    , __f'x1 :: !(Tensor 1 a)
    , __alpha1 :: !(Tensor 0 a)
    }
    deriving (Typeable)
makeLenses ''NewtonRaphson

deriving instance (Show (Tensor 0 a), Show (Tensor 1 a)) => Show (NewtonRaphson a)

instance (ValidTensor a) => Has_x1 NewtonRaphson a where x1 = _x1
instance (ValidTensor a) => Has_fx1 NewtonRaphson a where fx1 = _fx1
instance (ValidTensor a) => Has_fx0 NewtonRaphson a where fx0 = _fx0
instance (ValidTensor a) => Has_f'x1 NewtonRaphson a where f'x1 = _f'x1
instance (ValidTensor a) => Has_stepSize NewtonRaphson a where stepSize = _alpha1

deriving instance (Typeable Matrix)

newtonRaphson :: 
    ( ValidTensor v
    , IsScalar (Scalar v)
    , Tensor 2 v ~ LA.Matrix (Scalar v)
    , Tensor 1 v ~ LA.Vector (Scalar v)
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    ) => (Tensor 1 v -> Tensor 0 v)
      -> (Tensor 1 v -> Tensor 1 v)
      -> (Tensor 1 v -> Tensor 2 v)
--       -> Tensor 1 v
      -> v
      -> [NewtonRaphson v -> History Bool]
      -> History (NewtonRaphson v)
newtonRaphson f f' f'' x0 = optimize
--     (projectOrthant <=< step_newtonRaphson f f' f'')
    (step_newtonRaphson f f' f'')
    $ NewtonRaphson
        { __x1 = mkTensor x0 
        , __fx1 = f $ mkTensor x0
        , __fx0 = infinity
        , __f'x1 = f' $ mkTensor x0
        , __alpha1 = 1
        }

projectOrthant opt1 = do
    mopt0 <- prevValueOfType opt1
    return $ case mopt0 of
        Nothing -> opt1
        Just opt0 -> set x1 (VG.zipWith go (opt0^.x1) (opt1^.x1)) $ opt1
    where
        go a0 a1 = if (a1>=0 && a0>=0) || (a1<=0 && a0<=0)
            then a1
            else 0

step_newtonRaphson :: 
    ( ValidTensor v
    , IsScalar (Scalar v)
    , Tensor 2 v ~ LA.Matrix (Scalar v)
    , Tensor 1 v ~ LA.Vector (Scalar v)
    , Ord (Scalar v)
    , Typeable (Scalar v)
    , Typeable v
    ) => (Tensor 1 v -> Tensor 0 v)
      -> (Tensor 1 v -> Tensor 1 v)
      -> (Tensor 1 v -> Tensor 2 v)
      -> NewtonRaphson v
      -> History (NewtonRaphson v)
step_newtonRaphson f f' f'' opt = do
    let x0 = opt^.x1
        fx0 = opt^.fx1
        f'x0 = opt^.f'x1
        f''x0 = f'' x0 
        alpha0 = opt^.stepSize
    
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

    return $ NewtonRaphson
        { __x1 = x1
        , __fx1 = f x1
        , __fx0 = opt^.fx1
        , __f'x1 = f' x1
        , __alpha1 = alpha
        }

-------------------------------------------------------------------------------

step_gradient getdir getstep f f' f'' opt = do
    dir <- getdir f f' f'' opt
    step <- getstep dir f f' f'' opt
    let x = opt^.x1 <> step .* dir
    return $ set x1 x opt 

quadraticLineSearch dir f f' f'' opt = do
    let g y = f $ (opt^.x1) <> y .* dir
    bracket <- LineMin.lineBracket g (opt^.stepSize/2) (opt^.stepSize*2)
    brent <- LineMin.brent g bracket
        [ lowerBound $ opt^.fx1
        , maxIterations 100
        ]
    return $ brent^.x1

dir_newtonRaphson f f' f'' x0 = dir_unsafeNewtonRaphson 1 f f' f'' x0

dir_unsafeNewtonRaphson :: 
    ( Tensor 2 v ~ LA.Matrix (Scalar v)
    , ValidTensor v
    ) => Tensor 0 v
      -> (Tensor 1 v -> Tensor 0 v)
      -> (Tensor 1 v -> Tensor 1 v)
      -> (Tensor 1 v -> Tensor 2 v)
      -> NewtonRaphson v 
      -> History (Tensor 1 v)
dir_unsafeNewtonRaphson reg f f' f'' opt = do
    let f'x0 = opt^.f'x1
        f''x0 = f'' (opt^.x1)
    return $ (-1) .* (LA.inv (f''x0 <> reg .* LA.eye (LA.rows f''x0)) `LA.mul` f'x0)
