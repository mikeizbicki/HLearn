{- LANGUAGE UnboxedTuples -}
module HLearn.Numeric.Recipes
    where

import Control.Applicative
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

-------------------------------------------------------------------------------
--

-- | The "Trace" class allows optimization procedures to return exactly 
-- Laws:
-- curValue $ initTrace a1 a0 = a1
-- prevValue $ initTrace a1 a0 = a0
-- curValue $ addTrace t a = a
-- prevValue $ addTrace t a = curValue t

class Trace a b where
    addTrace :: a b -> b -> a b
    curValue :: a b -> b
    prevValue :: a b -> b
    initTrace :: b -> b -> a b
    numitr :: a b -> Int

---------------------------------------

newtype DoTrace a = DoTrace [a]

instance Trace DoTrace a where

    {-# INLINE addTrace #-}
    addTrace (DoTrace xs) x = DoTrace $ x:xs

    {-# INLINE curValue #-}
    curValue (DoTrace (x:_)) = x
    curValue (DoTrace []) = error "curValue: DoTrace empty"

    {-# INLINE prevValue #-}
    prevValue (DoTrace (_:x:_)) = x
    prevValue _ = error "prevValue: DoTrace has no previous value"

    {-# INLINE initTrace #-}
    initTrace a1 a0 = DoTrace [a1,a0]

    numitr (DoTrace xs) = length xs

---------------------------------------

newtype DoTraceLimit (n::Nat) a = DoTraceLimit [a]

instance KnownNat n => Trace (DoTraceLimit n) a where

    {-# INLINE addTrace #-}
    addTrace (DoTraceLimit xs) x = DoTraceLimit $ take n $ x:xs 
        where
            n = fromIntegral $ natVal (Proxy :: Proxy n)

    {-# INLINE curValue #-}
    curValue (DoTraceLimit (x:_)) = x
    curValue (DoTraceLimit []) = error "curValue: DoTraceLimit empty"

    {-# INLINE prevValue #-}
    prevValue (DoTraceLimit (_:x:_)) = x
    prevValue _ = error "prevValue: DoTraceLimit has no previous value"

    {-# INLINE initTrace #-}
    initTrace a1 a0 = DoTraceLimit [a1,a0]

---------------------------------------

type NoTrace a = DoTraceLimit 2 a

-------------------------------------------------------------------------------

type StopCriteria opt = [DoTrace opt -> Bool]

optimize :: StopCriteria opt -> (opt -> opt) -> DoTrace opt -> DoTrace opt
optimize stop step opt = if or $ map ($ opt) stop
    then opt
    else optimize stop step $ addTrace opt $ step $ curValue opt

_stop_itr :: Int -> StopCriteria opt
_stop_itr n = [\opt -> numitr opt > n]


_stop_tolerance :: 
    ( Fractional num
    , Ord num
    , Show num
    ) => (tmp -> num) -- | gets f x at iteration t-1
      -> num -- | tolerance 
      -> [DoTrace tmp -> Bool]
_stop_tolerance _fx tol = [go]
    where
        go tmp =  trace ("fx="++show fx++"; fx_old="++show fx_old++"; left="++show left++"; right="++show right) $ 
    --     2 * abs (fx - fx_old) >= tol * ( abs fx + abs fx_old + 1e-18 )
            left <= right
            where
                left = 2 * abs (fx - fx_old) 
                right = tol * ( abs fx + abs fx_old + 1e-18 )
                fx = _fx $ curValue tmp
                fx_old = _fx $ prevValue tmp

-------------------------------------------------------------------------------

itr2 :: Show tmp => [tmp -> Bool] -> (tmp -> tmp) -> tmp -> tmp
itr2 !stop !step !tmp = if or $ map ($tmp) stop
    then tmp
    else itr2 stop step (step tmp)

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
