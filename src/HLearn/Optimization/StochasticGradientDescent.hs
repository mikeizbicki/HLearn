module HLearn.Optimization.StochasticGradientDescent
    where

import  Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra

squaredError y yhat = (y-yhat)*(y-yhat)/2
squaredError' y yhat = (y-yhat)

step_sgd eta f x dp = 
step_sgd_raw c eta x = x + scale eta (c x)
