{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_skiplist_sandbox (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/stewyslocum/.cabal/bin"
libdir     = "/Users/stewyslocum/.cabal/lib/x86_64-osx-ghc-8.2.1/skiplist-sandbox-0.1.0.0-2E5fcefYj03L88RJCcEim"
dynlibdir  = "/Users/stewyslocum/.cabal/lib/x86_64-osx-ghc-8.2.1"
datadir    = "/Users/stewyslocum/.cabal/share/x86_64-osx-ghc-8.2.1/skiplist-sandbox-0.1.0.0"
libexecdir = "/Users/stewyslocum/.cabal/libexec"
sysconfdir = "/Users/stewyslocum/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "skiplist_sandbox_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "skiplist_sandbox_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "skiplist_sandbox_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "skiplist_sandbox_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "skiplist_sandbox_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "skiplist_sandbox_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
