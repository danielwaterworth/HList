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
  cabalRepl @ (ec, _, _) <- readProcessWithExitCode "cabal" args stdin
  if ec == ExitSuccess then return cabalRepl
    else do
      runghcRepl @ (ec2, _, _) <- readProcessWithExitCode "runghc" ("Setup.hs":args) stdin
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
