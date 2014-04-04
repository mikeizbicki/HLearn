module HLearn.Optimization.Conic
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Data.List
import Data.List.Extras
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
import Data.Random.Normal

import HLearn.Algebra
import qualified HLearn.Optimization.Common as Recipe
import qualified HLearn.Optimization.LineMinimization as LineMin

-------------------------------------------------------------------------------

conicprojection :: Matrix Double -> Matrix Double
conicprojection m = cmap realPart $ u LA.<> lambda' LA.<> trans u 
    where
        lambda' = cmap (\x -> if realPart x < 0 then 0 else x) lambda
        lambda = diagRect 0 l (VG.length l) (VG.length l)
        (l,u) = eig m

-------------------------------------------------------------------------------

data RandomConicPersuit a = RandomConicPersuit
    { _stdgen :: !StdGen
    , _soln :: !a
    , _fx :: !(Scalar a)
    , _solnlast :: !a
    }

-- data OptInfo a = OptInfo
--     { _stdgen :: !StdGen
--     , _x :: !a
--     , _fx :: 
--     , _xold :: !a
--     }

itr :: Int -> (tmp -> a) -> (tmp -> Bool) -> (tmp -> tmp) -> tmp -> a
itr i result stop step init = --trace ("i="++show i++"; fx="++show (_fx init)) $ --trace ("i="++show i++"; init="++show init) $ 
  if i==0 || stop init
    then result init
    else itr (i-1) result stop step (step init)

randomConicPersuit f x0 = _soln $ itr 10 id (\x -> False) (step_RandomConicPersuit f) init
    where
        init = RandomConicPersuit
            { _stdgen = mkStdGen $ round $ sumElements x0
            , _soln = x0
            , _fx = f x0
            , _solnlast = x0
            }

conicProjection f x0 = _soln $ argmin _fx [itr 1 id (\x -> False) (step_RandomConicPersuit f) (init i) | i <- [0..10]]
    where
        init i = RandomConicPersuit
            { _stdgen = mkStdGen $ i+(round $ sumElements x0)
            , _soln = x0
            , _fx = f x0
            , _solnlast = x0
            }

stop_RandomConicPersuit (RandomConicPersuit stdgen soln fx solnlast) = undefined

step_RandomConicPersuit f (RandomConicPersuit stdgen soln fx solnlast) = --trace ("lambda = "++show lambda++ "; phi="++show phi) $ 
    ret
    where
--         (x', stdgen') = runRand ((LA.fromList . take (rows soln)) `liftM` getRandomRs (-1,1)) stdgen
        normL 0 (xs,g) = (xs,g)
        normL i (xs,g) = normL (i-1) (x:xs,g')
            where
                (x,g') = normal g

        (x'std, stdgen') = normL (rows soln) ([],stdgen)
        x' = VS.fromList x'std
--         (lambda,phi::Matrix Double) = eigSH soln
--         q = (diag $ cmap (sqrt.abs) lambda) LA.<> phi
--         x' = (scale (1-kappa) q - scale kappa (ident (rows q))) LA.<> LA.fromList x'std
--         kappa = 1e-4

        y' = asColumn x' LA.<> asRow x'
--         y' = asColumn (LA.fromList x') LA.<> asRow (LA.fromList x')

        g_alpha alpha = f $ scale alpha y' + soln
        alpha_hat = error "step_randomConic" -- LineMin._x $ runOptimization $ LineMin.brent g_alpha (LineMin.lineBracket g_alpha 0 1)

        alpha_hat_y' = scale alpha_hat y'
        g_beta beta = f $ alpha_hat_y' + scale beta soln
        beta_hat = error "step_randomConic" -- LineMin._x $ LineMin.brent g_beta (LineMin.lineBracket g_beta 0 1)

        soln' = alpha_hat_y' + scale beta_hat soln

        ret = RandomConicPersuit
            { _stdgen = stdgen'
            , _soln = soln'
            , _fx = f soln'
            , _solnlast = soln
            }

