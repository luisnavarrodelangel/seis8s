{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cumbia (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lui/.cabal/bin"
libdir     = "/home/lui/.cabal/lib/x86_64-linux-ghc-8.6.5/cumbia-0.0.0.1-AgRXiMkQz1oF2W2stveRXb"
dynlibdir  = "/home/lui/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/lui/.cabal/share/x86_64-linux-ghc-8.6.5/cumbia-0.0.0.1"
libexecdir = "/home/lui/.cabal/libexec/x86_64-linux-ghc-8.6.5/cumbia-0.0.0.1"
sysconfdir = "/home/lui/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cumbia_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cumbia_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cumbia_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cumbia_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cumbia_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cumbia_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
