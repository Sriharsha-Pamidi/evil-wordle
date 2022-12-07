{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_wordle (
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
version = Version [0,2,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/bin"
libdir     = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/lib/aarch64-osx-ghc-8.10.7/wordle-0.2.2-FrIws47fvjZ7XbDgUX377A-wordle"
dynlibdir  = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/share/aarch64-osx-ghc-8.10.7/wordle-0.2.2"
libexecdir = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/libexec/aarch64-osx-ghc-8.10.7/wordle-0.2.2"
sysconfdir = "/Users/adityaaddepalli/Documents/230/evil-wordle/.stack-work/install/aarch64-osx/306116c3a841c392bcc2cadf7e73c18f40da031bac507448eaf914b949379bb1/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wordle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wordle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wordle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wordle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wordle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wordle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
