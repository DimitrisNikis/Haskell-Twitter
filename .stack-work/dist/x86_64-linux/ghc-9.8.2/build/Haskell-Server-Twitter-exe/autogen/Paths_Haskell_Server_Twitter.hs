{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Haskell_Server_Twitter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/bin"
libdir     = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/lib/x86_64-linux-ghc-9.8.2/Haskell-Server-Twitter-0.1.0.0-1XdzDoW8BjB53pUl7JqLM2-Haskell-Server-Twitter-exe"
dynlibdir  = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/lib/x86_64-linux-ghc-9.8.2"
datadir    = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/share/x86_64-linux-ghc-9.8.2/Haskell-Server-Twitter-0.1.0.0"
libexecdir = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/libexec/x86_64-linux-ghc-9.8.2/Haskell-Server-Twitter-0.1.0.0"
sysconfdir = "/home/dimitriinikitin/Desktop/Haskell/Haskell-Server-Twitter/.stack-work/install/x86_64-linux/2615efb76b8b1808fa0858b97e4e1e0dd0f0be4b9856eb1b00f17e5a973380ce/9.8.2/etc"

getBinDir     = catchIO (getEnv "Haskell_Server_Twitter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Haskell_Server_Twitter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Haskell_Server_Twitter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Haskell_Server_Twitter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell_Server_Twitter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell_Server_Twitter_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
