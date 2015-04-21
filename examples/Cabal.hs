{-# LANGUAGE CPP #-}
module Cabal where

import System.Process
import System.Exit
import Control.Monad

-- | > cabal args stdin = readProcessWithExitCode "cabal" args stdin
--
-- except it also tries to runghc Setup which can succeed (because
-- if things have been configured with runghc Setup, then a different
-- version of the Cabal library is chosen and cabal complains instead
-- of reconfiguring / using an older Cabal library
cabal :: [String] -> String
  -> IO (ExitCode, String, String)
cabal args stdin = do
#if __GLASGOW_HASKELL__ > 707
  cabalRepl @ (ec, _, _) <- readProcessWithExitCode "cabal" args stdin
  if ec == ExitSuccess then return cabalRepl
    else do
      runghcRepl @ (ec2, _, _) <- readProcessWithExitCode "runghc" ("Setup.lhs":args) stdin
      when (ec2 /= ExitSuccess) $ do
        putStrLn "Could not \"cabal ...\" (exitCode,stdout,stderr):"
        print cabalRepl
        putStrLn "Could not \"runghc Setup ...\" (exitCode,stdout,stderr):"
        print runghcRepl

        putStrLn "\"...\" above is "
        putStrLn "Command:"
        print ("cabal" : args)
        putStrLn "stdin:"
        print stdin
        return ()

      return runghcRepl
#else
  -- we don't have a cabal repl (at least if we're using the Cabal that ghc
  -- comes with), so read a file written by the ./Setup build
  "repl" : tgt : args <- return args
  ghc : rest <- lines `fmap` readFile ("dist/build/autogen/Build_" ++ tgt ++ ".flags")
  let args2 = "--interactive"
                                : "-v"
                                : "-package-db" : "dist/package.conf.inplace"
                                : concatMap words rest ++ args

      args3 = case break (=="--ghc-options") args2 of
                  (a, _:_:b) -> a ++ b
  print args3
  readProcessWithExitCode ghc args3 stdin
#endif
