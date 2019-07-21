{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_book (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/bin"
libdir     = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/lib/x86_64-linux-ghc-8.2.2/book-0.0.1-4RL63Lpfy7bIsM1EnYDCbG"
dynlibdir  = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/share/x86_64-linux-ghc-8.2.2/book-0.0.1"
libexecdir = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/libexec/x86_64-linux-ghc-8.2.2/book-0.0.1"
sysconfdir = "/home/gabriel/dev/workspaces/haskell-exercises/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.12/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "book_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "book_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "book_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "book_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "book_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "book_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
