{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_parser (
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

bindir     = "/home/gabriel/.cabal/bin"
libdir     = "/home/gabriel/.cabal/lib/x86_64-linux-ghc-8.2.2/parser-0.1.0.0-61FBufXPl9ZIS5ABNNk50u-parser"
dynlibdir  = "/home/gabriel/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/gabriel/.cabal/share/x86_64-linux-ghc-8.2.2/parser-0.1.0.0"
libexecdir = "/home/gabriel/.cabal/libexec/x86_64-linux-ghc-8.2.2/parser-0.1.0.0"
sysconfdir = "/home/gabriel/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
