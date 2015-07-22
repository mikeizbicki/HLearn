-- | This module let's us tune the cover tree's expansion ratio.
-- You might be able to get around 5% better performance by tuning this value to your specific application.
-- But you probably don't want to do this.
module HLearn.Data.SpaceTree.CoverTree.Unsafe
    ( setExprat
    , getExprat
    )
    where

import SubHask
import System.IO.Unsafe
import Data.IORef

--------------------------------------------------------------------------------

{-# NOINLINE expratIORef #-}
expratIORef = unsafePerformIO $ newIORef (1.3::Rational)

setExprat :: Rational -> IO ()
setExprat r = writeIORef expratIORef r

{-# INLINABLE getExprat #-}
getExprat :: Field r => r
getExprat = fromRational $ unsafePerformIO $ readIORef expratIORef

