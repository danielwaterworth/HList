import System.Process
import System.Exit
import System.IO
import Test.DocTest
import Data.Char

main = do
    o <- readProcess
        "cabal" ["repl","--ghc-options","-v0"]
        ":show packages\n:show language"
    let flags = words $ unlines $ filter ((=="-") . take 1 . dropWhile isSpace)
                    $ lines o

    doctest $ "-i.": "-idist/build/autogen": 
             "-optP-include":
             "-optPdist/build/autogen/cabal_macros.h" :
             "-Idist/build/autogen" : "-w": "Data/HList/CommonMain.hs": flags
