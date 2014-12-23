-- | For setting a constant step size.
module HLearn.Optimization.StepSize.Const
    ( lrConst
    , Hyperparams (..)
    )
    where

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)
import HLearn.History
import HLearn.Optimization.Common

lrConst :: VectorSpace v => Hyperparams v
lrConst = Hyperparams
    { step = 0.001
    }

newtype Hyperparams v = Hyperparams
    { step :: Scalar v
    }

data Params v = Params

instance VectorSpace v => LearningRate Hyperparams Params v where
    lrInit _ _ = Params

    lrStep (Hyperparams step) _ _ = return Params

    lrApply (Hyperparams step) _ v = step *. v


