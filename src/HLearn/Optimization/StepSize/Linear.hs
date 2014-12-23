-- | This is the classic formula for adjusting the learning rate from the original 1951 paper "A stochastic approximation method".
-- Assymptotically, it has optimal convergence on strongly convex functions.
-- But in practice setting good hyperparameters is difficult, and many important functions are not strongly convex.
module HLearn.Optimization.StepSize.Linear
    ( lrLinear
    , eta
    , gamma
    , Hyperparams (..)
    )
    where

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)
import HLearn.History
import HLearn.Optimization.Common

{-
-- | This is a slight generalization of "lrLinear" taken from the paper "Stochastic Gradient Descent Tricks"
lrBottou ::
    ( Floating (Scalar v)
    ) => Scalar v -- ^ the initial value
      -> Scalar v -- ^ the rate of decrease (recommended to be the smallest eigenvalue)
      -> Scalar v -- ^ the exponent;
                  -- this should be negative to ensure the learning rate decreases;
                  -- setting it to `-1` gives "lrLinear"
      -> LearningRate v
lrBottou gamma lambda exp = do
    t <- currentItr
    return $ \v -> v .* (gamma * ( 1 + gamma * lambda * t) ** exp)
-}

lrLinear :: VectorSpace v => Hyperparams v
lrLinear = Hyperparams
    { eta = 0.001
    , gamma = 0.1
    }

data Hyperparams v = Hyperparams
    { eta   :: !(Scalar v) -- ^ the initial value for the step size
    , gamma :: !(Scalar v) -- ^ the rate of decrease
    }

newtype Params v = Params
    { step :: Scalar v
    }

instance VectorSpace v => LearningRate Hyperparams Params v where
    lrInit (Hyperparams eta gamma) _ = Params eta

    lrStep (Hyperparams eta gamma) _ _ = do
        t <- currentItr
        return $ Params $ eta / (1 + gamma * t)

    lrApply _ (Params r) v = r *. v

