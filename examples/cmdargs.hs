{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import System.Console.CmdArgs
import Data.HList.CommonMain
import Data.Generics


{-

An example showing off the data instance for Record

Also a use of cmdArgs


-}

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
