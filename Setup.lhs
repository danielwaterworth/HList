#!/usr/bin/runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Language.Haskell.Extension
    ( Extension(EnableExtension, UnknownExtension, DisableExtension) )
import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package
    ( PackageName(PackageName),
      PackageId,
      InstalledPackageId,
      packageVersion,
      packageName )
import Distribution.PackageDescription ()
import Distribution.Simple
    ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils
    ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ()
import Distribution.Simple.LocalBuildInfo ()
import System.FilePath ( (</>) )
import Distribution.Verbosity ( Verbosity )
import Distribution.PackageDescription ()
import Distribution.Simple.LocalBuildInfo ()
import Distribution.PackageDescription
    ( TestSuite(testName),
      PackageDescription,
      Library(libBuildInfo),
      allExtensions )
import Distribution.Simple.Compiler ()
import Distribution.Simple.Setup
    ( BuildFlags(buildVerbosity),
      ConfigFlags(..),
      Flag(Flag, NoFlag),
      fromFlag )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(configFlags),
      ComponentLocalBuildInfo(componentPackageDeps),
      withTestLBI,
      withLibLBI )
import Data.Maybe

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \ lib libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".flags") $ unlines $
        [ hc , unwords (formatdeps (testDeps libcfg suitecfg) ++ exts lib) ]
  where
    hc = case configHcPath (configFlags lbi) of
            Flag a -> a
            NoFlag -> fromMaybe "ghc" (lookup "ghc" (configProgramPaths (configFlags lbi)))
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> "-package=" ++ n ++ "-" ++ showVersion (packageVersion p)

    exts lib = [ "-X" ++ se
                    | e <- allExtensions (libBuildInfo lib),
                      Just se <- [case e of
                        EnableExtension x -> Just (show x)
                        UnknownExtension x -> Just x
                        DisableExtension x -> Just ("No" ++ show x)
                  ]
                ]

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys


\end{code}
Adapted from:

Copyright 2012-2015 Edward Kmett

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
