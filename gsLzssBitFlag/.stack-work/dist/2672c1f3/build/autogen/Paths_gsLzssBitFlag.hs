module Paths_gsLzssBitFlag (
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
version = Version [0,2,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\!!\\gsLzssBitFlag\\.stack-work\\install\\8851aa27\\bin"
libdir     = "D:\\!!\\gsLzssBitFlag\\.stack-work\\install\\8851aa27\\lib\\x86_64-windows-ghc-7.10.3\\gsLzssBitFlag-0.2.0.0-6ZlkkGaKkNE5ehSkCZZUx3"
datadir    = "D:\\!!\\gsLzssBitFlag\\.stack-work\\install\\8851aa27\\share\\x86_64-windows-ghc-7.10.3\\gsLzssBitFlag-0.2.0.0"
libexecdir = "D:\\!!\\gsLzssBitFlag\\.stack-work\\install\\8851aa27\\libexec"
sysconfdir = "D:\\!!\\gsLzssBitFlag\\.stack-work\\install\\8851aa27\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gsLzssBitFlag_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gsLzssBitFlag_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "gsLzssBitFlag_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gsLzssBitFlag_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gsLzssBitFlag_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
