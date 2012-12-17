module Paths_HLearn (
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
libdir     = "/home/user/.cabal/lib/HLearn-0.0.1/ghc-7.6.1"
datadir    = "/home/user/.cabal/share/HLearn-0.0.1"
libexecdir = "/home/user/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "HLearn_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "HLearn_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "HLearn_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "HLearn_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
