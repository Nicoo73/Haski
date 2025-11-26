{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_space_game (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/marcelo/.cabal/bin"
libdir     = "/home/marcelo/.cabal/lib/x86_64-linux-ghc-8.8.4/space-game-1.0-inplace-game"
dynlibdir  = "/home/marcelo/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/marcelo/.cabal/share/x86_64-linux-ghc-8.8.4/space-game-1.0"
libexecdir = "/home/marcelo/.cabal/libexec/x86_64-linux-ghc-8.8.4/space-game-1.0"
sysconfdir = "/home/marcelo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "space_game_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "space_game_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "space_game_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "space_game_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "space_game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "space_game_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
