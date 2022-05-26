{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_cryptol_polymorphic_testing (
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

bindir     = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/bin"
libdir     = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/lib/x86_64-osx-ghc-9.0.2/cryptol-polymorphic-testing-0.1.0.0-1YeCOtQzulRKM6tiPfmqPM-cryptol-polymorphic-testing"
dynlibdir  = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/share/x86_64-osx-ghc-9.0.2/cryptol-polymorphic-testing-0.1.0.0"
libexecdir = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/libexec/x86_64-osx-ghc-9.0.2/cryptol-polymorphic-testing-0.1.0.0"
sysconfdir = "/Users/hblanchette/Documents/cryptol-polymorphic-testing/.stack-work/install/x86_64-osx/d50c42e792fab717275d095ff9d5d3e36f0b60f006f4d6e134b1271692f15c49/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cryptol_polymorphic_testing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cryptol_polymorphic_testing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cryptol_polymorphic_testing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cryptol_polymorphic_testing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cryptol_polymorphic_testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cryptol_polymorphic_testing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
