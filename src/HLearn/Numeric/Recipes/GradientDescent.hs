module HLearn.Numeric.Recipes.GradientDescent
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

import HLearn.Algebra
import HLearn.Numeric.Recipes
import qualified HLearn.Numeric.Recipes.LineMin as LineMin


-- g x = (x - 3) * (x + 5) * (x - 12) * (x+1)
-- g2 x = (x - 3) * (x+1)

step_NewtonRaphson_constrained !f !f' !f'' !tmp = OptTmp
    { __x = x'
    , __fx = f x'
    , __x_old = x
    , __fx_old = __fx tmp
    , __itr = __itr tmp+1
    }
    where
        x = __x tmp
--         x' = x - inv (f'' x) LA.<> f' x
        dir = scale (-1) $ reshape (rows x) (flatten $ inv (f'' x) LA.<> f' x)
        g y = f $ x + scale y dir
        alpha = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1)
        x' = x + scale alpha dir
--         x' = x + dir

step_NewtonRaphson_unconstrained !f !f' !f'' !tmp = OptTmp
    { __x = x'
    , __fx = f x'
    , __x_old = x
    , __fx_old = __fx tmp
    , __itr = __itr tmp+1
    }
    where
        x = __x tmp
--         x' = x - inv (f'' x) LA.<> f' x
        dir = scale (-1) $ reshape (rows x) (flatten $ inv (f'' x) LA.<> f' x)
        g y = f $ x + scale y dir
        alpha = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1)
--         x' = x + scale alpha dir
        x' = x + dir

newtonRaphson_constrained !f !f' !f''  !x0 = if __fx res > __fx_old res then __x res else __x_old res
    where
        res = itr2
            ( 
                [ stop_itr __itr 10
                , stop_tolerance __fx __fx_old 1e-12
                ]
            )
            (step_NewtonRaphson_constrained f f' f'') 
            (step_NewtonRaphson_constrained f f' f'' init)
        init = OptTmp x0 (f x0) x0 (f x0) 0

newtonRaphson_unconstrained !f !f' !f''  !x0 = if __fx res > __fx_old res {- || __fx res == infinity -} then __x res else __x_old res
    where
        res = itr2
            ( 
                [ stop_itr __itr 10
                , stop_tolerance __fx __fx_old 1e-12
                ]
            )
            (step_NewtonRaphson_unconstrained f f' f'') 
            (step_NewtonRaphson_unconstrained f f' f'' init)
        init = OptTmp x0 (f x0) x0 (f x0) 0

data OptTmp a = OptTmp
    { __x :: !a
    , __fx :: !(Ring a)
    , __x_old :: !a
    , __fx_old :: !(Ring a)
    , __itr :: Int
    }

-------------------------------------------------------------------------------

-- itr2 :: Show tmp => (tmp -> Bool) -> (tmp -> tmp) -> tmp -> tmp
-- itr2 !stop !step !tmp =  -- trace ("tmp="++show tmp) $ 
--   if stop tmp
--     then tmp
--     else itr2 stop step (step tmp)

randomStart !reps !f !opt !x0 = deepseq optL $ trace ("map f optL="++show (map f optL)) $ argmin f optL
    where
        optL = map (opt . x) [1..reps]
        r = rows x0
        c = cols x0
--         x i = x0 + ( (r><c) $ randomRs (-0.01,0.01) (mkStdGen i) )
        x i = if ret /= trans ret
            then error $ show ret 
            else x0 + ret
            where
                ret = buildMatrix r c build
                xs = take c $ go $ randomRs (-0.1,0.1) (mkStdGen i)

                go ys = take r ys : go (drop r ys)

                build (i,j) = (xs !! i') !! j'
                    where
                        i' = min i j
                        j' = max i j


data CGD a = CGD
    { ___x1 :: !a
    , ___fx1 :: !(Ring a)
    , ___f'x1 :: !a
    , ___f'x0 :: !a
    , ___s0 :: !a
    }

cgd f f' x0 = optimize
    (_stop_itr 1000 <> _stop_tolerance ___fx1 1e-2)
    (step_cgd LineSearch PolakRibiere f f')
    (initTrace cgd1 cgd0)
    where
        cgd1 = step_cgd LineSearch None f f' cgd0

        cgd0 = CGD
            { ___x1 = x0
            , ___fx1 = f x0
            , ___f'x1 = f' x0
            , ___f'x0 = f' x0
            , ___s0 = f' x0
            }

-- | Selects a method for choosing the step size
data StepMethod
    = StepSize Double
    | LineSearch

-- | method for determining the conjugate direction; See <https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient>
data ConjugateMethod
    = None
    | FletcherReeves
    | PolakRibiere
    | HestenesStiefel


step_cgd stepMethod conjMethod f f' (CGD x1 fx1 f'x1 f'x0 s0) = 
  trace ("fx1="++show fx1++"; beta="++show beta++"; alpha="++show alpha) $ CGD
    { ___x1 = x
    , ___fx1 = f x
    , ___f'x1 = f' x
    , ___f'x0 = f'x1
    , ___s0 = s1
    }
    where
        beta = max 0 $ case conjMethod of
            None -> 0
            FletcherReeves -> (sumElements $ f'x1 * f'x1) / (sumElements $ f'x0 * f'x0)
            PolakRibiere -> (sumElements $ f'x1*(f'x1-f'x0)) / (sumElements $ f'x0 * f'x0)
            HestenesStiefel -> -(sumElements $ f'x1*(f'x1-f'x0)) / (sumElements $ s0 * (f'x1-f'x0))

        s1 = -f'x1 + scale beta f'x1
        g y = f $ x1 + scale y s1

        alpha = case stepMethod of
            StepSize x -> x
            LineSearch -> LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1)

        x = x1 + scale alpha s1

runOptimization :: DoTrace (CGD a) -> a
runOptimization (DoTrace (x:_)) = ___x1 $ x

