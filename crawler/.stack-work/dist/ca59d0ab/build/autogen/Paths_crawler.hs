{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_crawler (
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

bindir     = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\bin"
libdir     = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\lib\\x86_64-windows-ghc-8.0.2\\crawler-0.1.0.0-Annm8vHT08AKHY6xiJUgHQ"
dynlibdir  = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\share\\x86_64-windows-ghc-8.0.2\\crawler-0.1.0.0"
libexecdir = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\libexec"
sysconfdir = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\crawler\\.stack-work\\install\\60f100ec\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "crawler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "crawler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "crawler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "crawler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "crawler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "crawler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
