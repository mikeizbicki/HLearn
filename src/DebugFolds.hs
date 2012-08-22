{-# LANGUAGE BangPatterns #-}

module DebugFolds
    where

import Control.Monad
import Control.Monad.Random
import Data.List
import System.Console.ANSI
import System.IO.Unsafe

import qualified Data.Foldable as F
   

foldlMTrace :: (F.Foldable f, Monad m) => (a -> b -> m a) -> a -> f b -> m a
foldlMTrace f x ys = unsafePerformIO $ do
    cursorDown 1
    return $ liftM snd $ F.foldlM trace_f (0,x) ys
    where
        trace_f !(itr,x) !y = unsafePerformIO $ do
            clearLine
            putStrLn $ "foldlMTrace.itr="++show itr{-++"/"++show len-}
            cursorUp 1
            let ret = f x y
            seq ret $ return $ liftM (\a->(itr+1,a)) ret
--         len = length ys
        
foldlTrace' :: (F.Foldable f) => (a -> b -> a) -> a -> f b -> a
foldlTrace' f x ys = unsafePerformIO $ do
    cursorDown 1
    return $ snd $ F.foldl' trace_f (0,x) ys
    where
        trace_f (itr,x) y = unsafePerformIO $ do
            clearLine
            putStrLn $ "foldlTrace'.itr="++show itr{-++"/"++show len-}
            cursorUp 1
            return $ (itr+1,f x y)
{-    where
        trace_f (itr,x) y = unsafePerformIO $ do
            clearLine
            putStrLn $ "itr="++show itr++"/"++show len
            cursorUp 1
            return $ liftM (\a->(itr+1,a)) $ f x y
        len = length ys-}
        
-- foldMTrace f x ys = foldMTraceItr 0 (length ys) f x ys
--     where
--         foldMTraceItr !itr !len f !x [] = return $! x
--         foldMTraceItr !itr !len f !x (y:ys) = do
-- --             fm <- f x y
--             fm <- if {-True -- -}(itr `mod` 10==0)
--                      then trace ("itr="++show itr++"/"++show len) $ f x y
--                      else let z = f x y in z `seq` z
--             foldMTraceItr (itr+1) len f fm ys
