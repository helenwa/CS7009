module Paths_blob (
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

bindir     = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\.stack-work\\install\\18bd06a5\\bin"
libdir     = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\.stack-work\\install\\18bd06a5\\lib\\x86_64-windows-ghc-7.10.3\\blob-0.1.0.0-3Io9c1JF9Yx5KmxhulkHmf"
datadir    = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\.stack-work\\install\\18bd06a5\\share\\x86_64-windows-ghc-7.10.3\\blob-0.1.0.0"
libexecdir = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\.stack-work\\install\\18bd06a5\\libexec"
sysconfdir = "C:\\Users\\happy\\Documents\\5thYear\\NetworkApplications\\codeblobs\\.stack-work\\install\\18bd06a5\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "blob_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "blob_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "blob_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "blob_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "blob_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
