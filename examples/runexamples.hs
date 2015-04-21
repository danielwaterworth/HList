module Main where

import Cabal
import Control.Exception
import System.FilePath
import Test.Hspec
import System.Exit
import System.Process
import System.Directory
import Data.Maybe
import Control.Monad

main = do
  es <- getDirectoryContents "examples"
  print es
  -- very dumb
  es <- filterM (\e -> allM
    [return (takeExtension e == ".hs"),
     doesFileExist (dropExtension ("examples"</>e) ++ ".ref") ]) es

  print es

  hspec $ do
    mapM_ runghcwith es


runghcwith f = describe f $ it "ok" $
  do
    let ex = ("examples" </>)
    let inFile = ex (takeBaseName f)
        outFile = dropExtension inFile ++ ".out"
        errFile = dropExtension inFile ++ ".err"
        refFile = dropExtension inFile ++ ".ref"

    (ec, stdout, stderr) <- cabal
            ["repl","examples",
              "-v0", "--ghc-options", "-w -fcontext-stack=50 -iexamples -v0"]
              (":set -i\n:set -iexamples\n:load " ++ inFile ++ "\nmain")

    writeFile outFile stdout

    ofe <- doesFileExist refFile
    diff <- if ofe
      then fmap Just $
        readProcess "diff" ["-b", outFile, refFile] ""
          `finally` writeFile errFile stderr
      else return Nothing

    unless (diff == Just "") $ writeFile errFile stderr

    return (ec, stderr, diff)
 `shouldReturn` (ExitSuccess, "", Just "")



allM [] = return True
allM (x:xs) = do
    x <- x
    if x then allM xs else return False
