{- LANGUAGE UnboxedTuples -}
module HLearn.Numeric.Recipes
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
import qualified HLearn.Numeric.Recipes.LineMin as LineMin



stop_itr :: 
    (tmp -> Int) -- | gets number of iterations
    -> Int -- | maximum numver of iterations
    -> tmp  
    -> Bool
stop_itr _itr n tmp = trace ("itr="++show itr) $ itr >= n
    where
        itr = _itr tmp

stop_tolerance :: 
    ( Fractional num
    , Ord num
    , Show num
    ) => (tmp -> num) -- | gets f x at iteration t-1
      -> (tmp -> num) -- | get  f x at iteration t
      -> num -- | tolerance 
      -> tmp -- | intermediate data structure 
      -> Bool
stop_tolerance _fx _fx_old tol tmp = trace ("fx="++show fx++"; fx_old="++show fx_old++"; left="++show left++"; right="++show right) $ 
--     2 * abs (fx - fx_old) >= tol * ( abs fx + abs fx_old + 1e-18 )
    left <= right
    where
        left = 2 * abs (fx - fx_old) 
        right = tol * ( abs fx + abs fx_old + 1e-18 )
        fx = _fx tmp
        fx_old = _fx_old tmp


instance Num r => HasRing (Vector r) where
    type Ring (Vector r) = r

instance Num r => HasRing (Matrix r) where
    type Ring (Matrix r) = r
