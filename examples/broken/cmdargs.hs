{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
import System.Console.CmdArgs
import Data.HList.CommonMain
import Data.Generics

makeLabels6 (words "x y z")


d0 = x .=. (5 :: Int)
    .*. y .=. True
    .*. z .=. False
    .*. emptyRecord


data E = E { a :: Int, b, c :: Bool }
    deriving (Show, Data, Typeable)
e0 = E 5 True False

main = do
    print d0
    print $ gmapT (mkT not) d0
    print $ gmapT (mkT (+(1::Int))) d0

    print $ fromConstrB (undefined `extB` (1::Int) `extB` True) undefined `asTypeOf` d0

    rc <- cmdArgs d0
    print rc

{- Some things work properly:


> :main --help
Record{x=5,y=True,z=False}
Record{x=5,y=False,z=True}
Record{x=6,y=True,z=False}
Record{x=1,y=True,z=True}
The record program

record [OPTIONS]

Common flags:
  -x=INT          
  -y              
  -z              
  -?     --help     Display help message
  -V     --version  Print version information
*** Exception: ExitSuccess


But this doesn't:

*Main> :main -x 3
Record{x=5,y=True,z=False}
Record{x=5,y=False,z=True}
Record{x=6,y=True,z=False}
Record{x=1,y=True,z=True}
Record{*** Exception: <<loop>>

-}
