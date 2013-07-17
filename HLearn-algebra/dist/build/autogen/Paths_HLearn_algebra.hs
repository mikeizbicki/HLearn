module Paths_HLearn_algebra (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0,2,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/m18/.cabal/bin"
libdir     = "/home/m18/.cabal/lib/HLearn-algebra-1.0.2.1/ghc-7.6.3"
datadir    = "/home/m18/.cabal/share/HLearn-algebra-1.0.2.1"
libexecdir = "/home/m18/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "HLearn_algebra_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HLearn_algebra_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HLearn_algebra_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HLearn_algebra_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
