{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_etna_stlc (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/bin"
libdir     = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/lib/aarch64-osx-ghc-9.0.2/etna-stlc-0.0.0-9Rgpr8NYX4ICr3KolDIbwh"
dynlibdir  = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/lib/aarch64-osx-ghc-9.0.2"
datadir    = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/share/aarch64-osx-ghc-9.0.2/etna-stlc-0.0.0"
libexecdir = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/libexec/aarch64-osx-ghc-9.0.2/etna-stlc-0.0.0"
sysconfdir = "/Users/akeles/Programming/projects/PbtBenchmark/etna2/workloads/Haskell/STLC/.stack-work/install/aarch64-osx/4f0a2e23ab5ced38d0c59d030489910952f1412068c84340cda7b286690ea7f0/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "etna_stlc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "etna_stlc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "etna_stlc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "etna_stlc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "etna_stlc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "etna_stlc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
