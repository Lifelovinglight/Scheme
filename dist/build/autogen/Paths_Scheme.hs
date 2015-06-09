module Paths_Scheme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/self/.cabal/bin"
libdir     = "/home/self/.cabal/lib/x86_64-linux-ghc-7.8.4/Scheme-0.1.0.0"
datadir    = "/home/self/.cabal/share/x86_64-linux-ghc-7.8.4/Scheme-0.1.0.0"
libexecdir = "/home/self/.cabal/libexec"
sysconfdir = "/home/self/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Scheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
