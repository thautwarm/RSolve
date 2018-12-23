{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Rcheck (
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

bindir     = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\bin"
libdir     = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\lib\\x86_64-windows-ghc-8.4.4\\Rcheck-0.1.0.0-7dPoA7xmmR0B0rRABDqxQU-Rcheck"
dynlibdir  = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\lib\\x86_64-windows-ghc-8.4.4"
datadir    = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\share\\x86_64-windows-ghc-8.4.4\\Rcheck-0.1.0.0"
libexecdir = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\libexec\\x86_64-windows-ghc-8.4.4\\Rcheck-0.1.0.0"
sysconfdir = "R:\\Redy\\Rcheck\\.stack-work\\install\\fd843c69\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Rcheck_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Rcheck_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Rcheck_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Rcheck_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Rcheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Rcheck_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
