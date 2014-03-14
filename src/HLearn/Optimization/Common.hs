{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds, NoImplicitPrelude, RebindableSyntax #-}

module HLearn.Optimization.Common
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer.Lazy
import Control.DeepSeq
import qualified Data.DList as DList
import Data.Dynamic
import Data.Typeable
import Debug.Trace

import HLearn.Algebra
-- import HLearn.Algebra.Structures.Monad
-- import qualified HLearn.Numeric.Recipes.LineMin as LineMin

-------------------------------------------------------------------------------

type OptMonad m = MonadWriter (DList.DList Dynamic) m

-- data Identity w a = Identity { runIdentity :: a }
-- 
-- instance Functor (Identity w) where
--     fmap f (Identity a) = Identity $ f a
-- 
-- -- instance Applicative Identity where
-- --     pure a = Identity a
-- --     (Identity f) <*> (Identity a) = Identity $ f a
-- 
-- instance Monad (Identity w) where
--     (Identity a) >>= f = f a
--     return a = Identity a
-- 
-- instance (Show w,Monoid w) => MonadWriter w (Identity w) where
--     tell w = trace ("w="++show w) $ return ()
-- 
-- instance Show (DList.DList Dynamic) where
--     show xs = show $ DList.toList xs

---------------------------------------

data TypeableLog a 
    = Cons 
        { runTypeableLog :: a
        , sub :: [TypeableLog Dynamic]
        , next :: TypeableLog a
        }
    | Nil

-- instance Functor TypeableLog where
--     fmap f (Cons a xs) = Cons (f a) xs
--     fmap f Nil = Nil
-- 
-- instance Monad TypeableLog where
--     (Cons a xs) >>= f = Cons a' (xs'++xs) 
--         where 
--             (Cons a' xs') = f a
-- 
--     Nil >>= f = Nil
--     return a = Cons a Nil

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
    deriving (Typeable)

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

-- optimize :: (Monad m) => StopCriteria opt -> (opt -> m opt) -> DoTrace opt -> m (DoTrace opt)
optimize :: 
--     ( ValidMonad m (DoTrace opt)
--     , ValidMonad m opt
    ( Monad m
    , MonadWriter (DList.DList Dynamic) m
    ) => StopCriteria opt 
      -> (opt -> m opt) 
      -> DoTrace opt 
      -> m (DoTrace opt)
optimize stop step opt = do
--   tell ["optimize"]
  if or $ map ($ opt) stop
    then return opt
    else do
        opt' <- step $ curValue opt
        optimize stop step $ addTrace opt opt'

-- optimize :: StopCriteria opt -> (opt -> opt) -> DoTrace opt -> DoTrace opt
-- optimize stop step opt = if or $ map ($ opt) stop
--     then opt
--     else optimize stop step $ addTrace opt $ step $ curValue opt

---------------------------------------

class Has_x1 opt v where x1 :: opt v -> v
class Has_fx1 opt v where fx1 :: opt v -> Scalar v
class Has_fx0 opt v where fx0 :: opt v -> Scalar v

-- runOptimization :: Has_x1 opt => Identity (DoTrace (opt v)) -> v
-- runOptimization = x1 . curValue . runIdentity

report :: (Typeable t, MonadWriter (DList.DList Dynamic) m) => t -> m t
report x = do
    tell $ DList.fromList [toDyn x]
    return x

runOptimization :: 
    ( Monad m
    , Has_x1 opt v
    ) => ([Dynamic] -> m ())
      -> Writer (DList.DList Dynamic) (DoTrace (opt v)) 
      -> m v
runOptimization proc m = do
    let (opt,dynlog) = runWriter m
        log = DList.toList dynlog
    tmp <- proc log
    seq tmp $ return $ x1 . curValue $ opt
             
traceOptimization :: forall opt v. 
    ( Has_x1 opt v
    , Has_fx1 opt v
    , Typeable (opt v)
    , Show (Scalar v)
    ) => Writer (DList.DList Dynamic) (DoTrace (opt v)) -> v
traceOptimization m = trace "traceOptimization" $ runIdentity $ runOptimization proc m
    where
        proc [] = return ()
        proc (x:xs) = do
            tmp <- case fromDynamic x :: Maybe (opt v) of
                Nothing -> return ()
                Just opt -> trace ("fx1 opt="++show (fx1 opt)) $ return ()
            seq tmp $ proc xs 


---------------------------------------

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
        go tmp = left <= right
            where
                left = 2 * abs (fx - fx_old) 
                right = tol * ( abs fx + abs fx_old + 1e-18 )
                fx = _fx $ curValue tmp
                fx_old = _fx $ prevValue tmp

-------------------------------------------------------------------------------

itr2 :: [tmp -> Bool] -> (tmp -> tmp) -> tmp -> tmp
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

