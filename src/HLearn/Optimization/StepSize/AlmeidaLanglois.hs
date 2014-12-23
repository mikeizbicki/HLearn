-- |
--
-- See: Almeida and Langlois, "Parameter Adaptation in Stochastic Optimization" equation 4.6
module HLearn.Optimization.StepSize.AlmeidaLanglois
    where

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)
import HLearn.History
import HLearn.Optimization.Common

import qualified Data.Vector.Generic as VG

-- lrAlmeidaLanglois ::
--     ( VectorSpace v
--     , VG.Vector u r
--     , u r ~ v
--     , Field r
--     ) => Scalar v
--       -> Scalar v
--       -> v
--       -> AlmeidaLanglois v
--       -> History (AlmeidaLanglois v)
-- lrAlmeidaLanglois k gamma grad AlmeidaLangloisInit = return $ AlmeidaLanglois
--     { d = grad
--     , v = VG.map (const 100) grad
--     , p = VG.map (const 0.1) grad
--     }
-- lrAlmeidaLanglois k gamma grad al = return $ AlmeidaLanglois
--     { d = grad
--     , v = v'
--     , p = p al - k *. p al .*. d al .*. grad ./. v'
--     }
--     where
--         v' = gamma *. v al + (1 - gamma) *. grad .*. grad

lrAlmeidaLanglois :: (VectorSpace (v r), VG.Vector v r) => Hyperparams (v r)
lrAlmeidaLanglois = Hyperparams
    { k     = 0.01
    , gamma = 0.9
    , u     = 1.01
    }

data Hyperparams v = Hyperparams
    { k     :: !(Scalar v)
    , gamma :: !(Scalar v)
    , u     :: !(Scalar v)
    }

-- | The criptic variable names in this data type match the variable names from the original paper
data Params v = Params
    { d :: !v -- ^ gradient
    , v :: !v -- ^ component-wise running average of the gradient squared
    , p :: !v -- ^ actual step size
    }

instance (r ~ Scalar (v r), VectorSpace (v r), VG.Vector v r) => LearningRate Hyperparams Params (v r) where
    lrInit (Hyperparams _ _ u) x = Params
        { d = VG.map (const $ 1/u) x
        , v = VG.map (const $ 1/u) x
        , p = VG.map (const $ 0.1) x
        }

    lrStep (Hyperparams k gamma _) (Params d0 v0 p0) grad = return $ Params
        { d = grad
        , v = v'
        , p = p0  + k *. p0 .*. d0 .*. grad ./. v'
        }
        where
            v' = gamma *. v0 + (1 - gamma) *. grad .*. grad

    lrApply _ (Params d v p) x = p .*. x
