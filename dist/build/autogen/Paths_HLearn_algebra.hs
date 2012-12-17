module Paths_HLearn_algebra (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/user/.cabal/bin"
libdir     = "/home/user/.cabal/lib/HLearn-algebra-0.0.1/ghc-7.6.1"
datadir    = "/home/user/.cabal/share/HLearn-algebra-0.0.1"
libexecdir = "/home/user/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "HLearn_algebra_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "HLearn_algebra_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "HLearn_algebra_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "HLearn_algebra_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
