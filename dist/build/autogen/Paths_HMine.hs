module Paths_HMine (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/user/.cabal/bin"
libdir     = "/home/user/.cabal/lib/HMine-0.0.0.1/ghc-7.4.1"
datadir    = "/home/user/.cabal/share/HMine-0.0.0.1"
libexecdir = "/home/user/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "HMine_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "HMine_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "HMine_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "HMine_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
