{- LANGUAGE UnboxedTuples -}
module HLearn.Numeric.Recipes
    where

import Control.Monad
import Control.Monad.ST
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import qualified HLearn.Numeric.Recipes.LineMin as LineMin

g x = (x - 3) * (x + 5) * (x - 12) * (x+1)
g2 x = (x - 3) * (x+1)


stepNewtonRaphson f f' x = x - f x / f' x

stepSteepestDescent f' eta x = x - eta * f' x

instance Num r => HasRing (Matrix r) where
    type Ring (Matrix r) = r

-------------------------------------------------------------------------------

conjugateGradientDescent f f' x0 = LineMin.itr 10000 _x stop_ConjugateGradientDescent (step_ConjugateGradientDescent f f') init
    where
        init = ConjugateGradientDescent x0 fx (f' x0) x0 (fx*fx)  
        fx = f x0

data ConjugateGradientDescent a = ConjugateGradientDescent { _x :: a, _fx :: Ring a, _dir :: a, _xlast :: a, _fxlast :: Ring a }

-- stop_ConjugateGradientDescent :: (HasRing a, Field (Ring a), Ord (Ring a)) => ConjugateGradientDescent a -> Bool
stop_ConjugateGradientDescent (ConjugateGradientDescent x fx dir xlast fxlast) = left <= right
    where
        left = 2*(abs $ fx-fxlast) 
        right = tol*(abs fxlast+abs fx+eps)
--         left = abs (fx-fxlast)
--         right = tol
--         eps = 1e-10
        tol = 1e-6

-- step_ConjugateGradientDescent :: ConjugateGradientDescent a -> ConjugateGradientDescent a
step_ConjugateGradientDescent f f' (ConjugateGradientDescent x fx dir xlast fxlast) = trace ("step_cgd; dist="++show dist++"; fx="++show fx++{-"; fxlast="++show fxlast++-}"; gradsize="++show gradsize) ret
    where
        g y = f $ x `LA.add` scale y dir'

--         dist = LineMin.gss g (LineMin.lineBracket g 0 1e-20)
        dist = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1e-20)

        beta = maxElement $ (trans f'x' LA.<> f'x') / (trans dir LA.<> dir)

        dir' = f'x' `LA.add` scale beta dir

        gradsize = sumElements f'x'

        f'x' = f' x

        x' = x `LA.add` scale dist f'x'

        ret = ConjugateGradientDescent
            { _x = x'
            , _fx = f x'
            , _dir = dir'
            , _xlast = x
            , _fxlast = fx
            }
