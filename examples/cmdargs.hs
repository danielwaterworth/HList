{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import System.Console.CmdArgs
import Data.HList.CommonMain
import Data.Generics


{-

An example showing off the data instance for Record

Also a use of cmdArgs

Note that ghc-7.8.2 does not have (or can produce) instances of typeable
for types of kind Symbol (ie. promoted strings):
<https://ghc.haskell.org/trac/ghc/ticket/9111>, so for now use the Label3
style

-}

-- makeLabels6 (words "x y z") -- works for ghc-7.6
makeLabels3 "examples_cmdargs" (words "x y z")


d0 = x .=. (5 :: Int)
    .*. y .=. True
    .*. z .=. False
    .*. emptyRecord


-- the equivalent ordinary record for reference
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

