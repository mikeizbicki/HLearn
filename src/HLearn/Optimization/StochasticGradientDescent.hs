-- |
--
-- Important references:
--
-- 1. Bach and Moulines, "Non-Asymptotic Analysis of Stochastic Approximation Algorithms for Machine Learning"
-- 2. Xu, "Towards optimal one pass large scale learning with averaged stochastic gradient descent"
--
module HLearn.Optimization.StochasticGradientDescent
--     (
--     )
    where

import SubHask

import HLearn.History
import HLearn.Optimization.Multivariate

import qualified Data.Vector.Generic as VG

-------------------------------------------------------------------------------

class LearningRate init step v | init -> step, step -> init where
    lrStep :: init v -> step v -> v -> History (step v)
    lrInit :: init v -> v -> step v
    lrApply :: init v -> step v -> v -> v

-------------------------------------------------------------------------------

data SGD container step dp v = SGD
    { __dataset :: !(container dp)
    , __x1      :: !v
    , __step    :: !(step v)
    }
    deriving Typeable

instance Show (SGD a b c d) where
    show _ = "SGD"

instance Has_x1 (SGD container step dp) v where x1 = __x1

type StochasticMethod container dp v
    = container dp -> (dp -> v -> v) -> History (v -> v)

randomSample ::
    ( VG.Vector container dp
    ) => StochasticMethod container dp v
randomSample !dataset !f = do
    t <- currentItr
    let a = 1664525
        c = 1013904223
    return $ f $ dataset VG.! ((a*t + c) `mod` VG.length dataset)

linearScan ::
    ( VG.Vector container dp
    ) => StochasticMethod container dp v
linearScan !dataset !f = do
    t <- currentItr
    return $ f $ dataset VG.! ( t `mod` VG.length dataset )

minibatch :: forall container dp v.
    ( VG.Vector container dp
    , Semigroup v
    ) => Int -> StochasticMethod container dp v
minibatch !size !dataset !f = do
    t <- currentItr
    let start = t `mod` VG.length dataset
        minidata = VG.slice start (min (VG.length dataset-start) size)  dataset

    let go (-1) ret = ret
        go j    ret = go (j-1) $ ret + (f $ minidata `VG.unsafeIndex` j)

    return $ go (VG.length minidata-1) id

---------------------------------------

stochasticGradientDescent ::
    ( VectorSpace v
    , LearningRate hyperparams params v
    , Typeable v
    , Typeable container
    , Typeable dp
    , Typeable params
    ) => StochasticMethod container dp v
      -> hyperparams v
      -> container dp
      -> (dp -> v -> v)
      -> v
      -> StopCondition_ (SGD container params dp v)
      -> History (SGD container params dp v)
stochasticGradientDescent !sm !lr !dataset !f' !x0 !stops = iterate
    ( stepSGD sm lr f' )
    ( SGD dataset x0 $ lrInit lr x0 )
    stops

stepSGD ::
    ( VectorSpace v
    , LearningRate hyperparams params v
    , Typeable v
    , Typeable container
    , Typeable dp
    , Typeable params
    ) => StochasticMethod container dp v
      -> hyperparams v
      -> (dp -> v -> v)
      -> SGD container params dp v
      -> History (SGD container params dp v)
stepSGD !sm !hyperparams !f' !sgd = do
    calcgrad <- sm (__dataset sgd) f'
    let grad = calcgrad $ __x1 sgd
    step <- lrStep hyperparams (__step sgd) grad
    report $ sgd
        { __x1 = __x1 sgd - (lrApply hyperparams step grad)
        , __step = step
        }
