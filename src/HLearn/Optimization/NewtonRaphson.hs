module HLearn.Optimization.NewtonRaphson
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
import HLearn.Optimization.Common
import qualified HLearn.Optimization.LineMinimization as LineMin

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
    , __fx :: !(Scalar a)
    , __x_old :: !a
    , __fx_old :: !(Scalar a)
    , __itr :: Int
    }

deriving instance (Show a, Show (Scalar a)) => Show (OptTmp a)

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

