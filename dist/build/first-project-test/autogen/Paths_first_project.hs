{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_first_project (
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

bindir     = "/home/mihai/.cabal/bin"
libdir     = "/home/mihai/.cabal/lib/x86_64-linux-ghc-8.2.2/first-project-0.1.0.0-3rUlqx1qHDlH9DtyFG8KV7-first-project-test"
dynlibdir  = "/home/mihai/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/mihai/.cabal/share/x86_64-linux-ghc-8.2.2/first-project-0.1.0.0"
libexecdir = "/home/mihai/.cabal/libexec/x86_64-linux-ghc-8.2.2/first-project-0.1.0.0"
sysconfdir = "/home/mihai/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "first_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "first_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "first_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "first_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "first_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "first_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
