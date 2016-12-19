module Paths_course (
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
version = Version [0,1,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chong/.cabal/bin"
libdir     = "/home/chong/.cabal/lib/x86_64-linux-ghc-7.10.3/course-0.1.4-HI6Tar9Fj7OCxNKMdi5IBz"
datadir    = "/home/chong/.cabal/share/x86_64-linux-ghc-7.10.3/course-0.1.4"
libexecdir = "/home/chong/.cabal/libexec"
sysconfdir = "/home/chong/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "course_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "course_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "course_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "course_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "course_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
