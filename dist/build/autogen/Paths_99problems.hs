{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_99problems (
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

bindir     = "/Users/jdeleon/.cabal/bin"
libdir     = "/Users/jdeleon/.cabal/lib/x86_64-osx-ghc-8.0.2/99problems-0.1.0.0"
dynlibdir  = "/Users/jdeleon/.cabal/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/jdeleon/.cabal/share/x86_64-osx-ghc-8.0.2/99problems-0.1.0.0"
libexecdir = "/Users/jdeleon/.cabal/libexec"
sysconfdir = "/Users/jdeleon/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "99problems_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "99problems_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "99problems_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "99problems_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "99problems_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "99problems_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
