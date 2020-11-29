{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_seis8s (
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
libdir     = "/home/lui/.cabal/lib/x86_64-linux-ghc-8.6.5/seis8s-0.0.0.1-25SeVcDHGU64A1M6vJs2R6"
dynlibdir  = "/home/lui/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/lui/.cabal/share/x86_64-linux-ghc-8.6.5/seis8s-0.0.0.1"
libexecdir = "/home/lui/.cabal/libexec/x86_64-linux-ghc-8.6.5/seis8s-0.0.0.1"
sysconfdir = "/home/lui/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "seis8s_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "seis8s_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "seis8s_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "seis8s_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "seis8s_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "seis8s_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
