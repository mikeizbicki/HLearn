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


g x = (x - 3) * (x + 5) * (x - 12) * (x+1)
g2 x = (x - 3) * (x+1)

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
            ( f_or 
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
            ( f_or 
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

itr2 :: Show tmp => (tmp -> Bool) -> (tmp -> tmp) -> tmp -> tmp
itr2 !stop !step !tmp =  -- trace ("tmp="++show tmp) $ 
  if stop tmp
    then tmp
    else itr2 stop step (step tmp)

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

f_or :: [a -> Bool] -> a -> Bool
f_or xs a = or $ map ($a) xs

conjugateGradientDescent !f !f' !x0 = _x $ itr2
    ( f_or 
        [ stop_itr _itr 100
        , stop_tolerance _fx _fxlast 1e-6
        ]
    )
    (step_ConjugateGradientDescent f f') 
    init
    where
        init = ConjugateGradientDescent 
            { _x = x'
            , _fx = f x'
            , _dir = dir'
            , _grad = dir'
            , _xlast = x0 
            , _fxlast = fx0
            , _nomove = 0
            , _itr = 0
            }

        dir' = scale (-1) $ f' x0
        alpha = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1e-2)
        g y = f $ x0 + scale y dir'
        x' = x0 + scale alpha dir'
                

        fx0 = f x0

data ConjugateGradientDescent a = ConjugateGradientDescent 
    { _x :: !a
    , _fx :: !(Ring a)
    , _dir :: !a
    , _grad :: !a
    , _xlast :: !a
    , _fxlast :: !(Ring a)
    , _nomove :: !Int
    , _itr :: !Int
    }

deriving instance (Show a, Show (Ring a)) => Show (ConjugateGradientDescent a)

-- stop_ConjugateGradientDescent :: (HasRing a, Field (Ring a), Ord (Ring a)) => ConjugateGradientDescent a -> Bool
stop_ConjugateGradientDescent (ConjugateGradientDescent x fx dir grad xlast fxlast nomove itr) = trace ("left="++show left++"; right="++show right++"; diff="++show (left-right)++"; fx="++show fx++"; fxlast="++show fxlast++"; gradsize="++show gradsize++"; nomove="++show nomove) $ 
    (nomove > 500 && left <= right) || fx > 1e200 || fx /= fx
    where
        left = 2*(abs $ fx-fxlast) 
        right = tol*(abs fxlast+abs fx+eps)
--         left = abs (fx-fxlast)
--         right = tol
--         eps = 1e-10
        tol = 1e-18
        gradsize = sumElements $ cmap abs dir

step_ConjugateGradientDescent f f' (ConjugateGradientDescent x fx dir grad xlast fxlast nomove itr) = 
    trace ("step_cgd; alphs="++show alpha++"; fx="++show fx++"; gradsize="++show gradsize++"; beta="++show beta) 
--     $ trace ("dir'="++show dir') 
    ret
    where
        delta_x = scale (-1) $ f' x
--         beta = 1::Double
        beta = (sumElements $ delta_x * delta_x)
             / (sumElements $ grad * grad)
--         beta = (sumElements $ (trans $ delta_x) LA.<> (delta_x - grad))
--              / (sumElements $ (trans $ grad) LA.<> grad)
        dir' = delta_x + scale beta dir
--         alpha = 0.01 
        alpha = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1e-2)
        g y = f $ x + scale y dir'
        x' = x + scale alpha dir'

        gradsize = sumElements $ cmap abs dir'

        ret = ConjugateGradientDescent
            { _x = x'
            , _fx = f x'
            , _dir = dir'
            , _grad = delta_x
            , _xlast = x
            , _fxlast = fx
            , _nomove = if fx==fxlast
                then nomove+1
                else 0
--                 else if nomove > 0
--                     then error "nomove reset"
--                     else 0
            , _itr = itr+1
            }

-- step_ConjugateGradientDescent f f' (ConjugateGradientDescent x fx dir xlast fxlast) = trace ("step_cgd; dist="++show dist++"; fx="++show fx++{-"; fxlast="++show fxlast++-}"; gradsize="++show gradsize++"; beta="++show beta) ret
--     where
--         g y = f $ x `LA.add` scale (y) dir'
-- 
-- --         dist = LineMin.gss g (LineMin.lineBracket g 0 1e-20)
--         dist = LineMin._x $ LineMin.brent g (LineMin.lineBracket g 0 1e-20)
-- 
-- --         betaraw = (f'x' LA.<> trans f'x') / ( dir LA.<> trans dir)
--         beta = (sumElements $ f'x' * f'x') / (sumElements $ dir * dir)
-- --         beta = sumElements $ betaraw
-- --         beta = maxElement $ (trans f'x' LA.<> (f'x' `LA.sub` dir)) / (trans dir LA.<> dir)
-- 
--         dir' = f'x' -- `LA.add` scale beta dir
-- 
--         gradsize = sumElements f'x'
-- 
--         f'x' = scale (-1) $ f' x
-- 
--         x' = x `LA.add` scale dist f'x'
-- 
--         ret = ConjugateGradientDescent
--             { _x = x'
--             , _fx = f x'
--             , _dir = dir'
--             , _xlast = x
--             , _fxlast = fx
--             }
