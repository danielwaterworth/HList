{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import System.Console.CmdArgs
import Data.HList.CommonMain
import Data.Generics
import Control.Lens
import System.Environment
import GHC.TypeLits (Symbol)
import Data.HList.Labelable(LabeledTo,ToSym)

{-

An example showing off the data instance for Record / Variant / TIP / TIC

Also a use of cmdArgs

Note that ghc-7.8.2 does not have (or can produce) instances of typeable
for types of kind Symbol (ie. promoted strings):
<https://ghc.haskell.org/trac/ghc/ticket/9111>, so for now use the Label3
style

-}

#define USE_LABEL3 __GLASGOW_HASKELL__ == 708

#if USE_LABEL3
makeLabels3 "examples_cmdargs" (words "x y z tic")
makeLabels3 "optV" (words "optA optB optC")
#else
-- works for ghc-7.6
makeLabels6 (words "x y z tic")
makeLabels6 (words "optA optB optC")
#endif

makeLabelable "abc df"

#if USE_LABEL3
-- XXX remove extra Label?
v = (optA .*. optB .*. optC .*. emptyProxy)
      `zipTagged` (Proxy :: Proxy '[Int,Char,Double])
#else
v = Proxy :: Proxy '[Tagged "optA" Int, Tagged "optB" Char, Tagged "optC" Double]
#endif

type Z' = TagR [Int, Char, Double]

d0 = x .=. (5 :: Int)
    .*. y .=. True
    .*. z .=. mkVariant optC (1 :: Double) v
    .*. tic .=. mkTIC' 'x' (Proxy :: Proxy Z')
    .*. emptyRecord

-- the equivalent ordinary record for reference
data E = E { a :: Int, b :: Bool }
    deriving (Show, Data, Typeable)

data Opt = OptA Int | OptB Char | OptC Double
    deriving (Show, Data, Typeable)

e0 = E 5 True

main = do
    print d0
    print $ gmapT (mkT ((+1) :: Double -> Double)) (mkVariant optC 1 v)
    print $  (mkVariant optC 1 v)

    print $ gmapT (mkT not) d0
    print $ gmapT (mkT (+(1::Int))) d0

    let theB :: Typeable a => a
        theB = error "theB"
              `extB` (1::Int)
              `extB` True
              `extB` (2.5::Double)
              `extB` 'b'
              `extB` mkVariant optC theB v
              `extB` mkTIC' (theB :: Char) (Proxy :: Proxy Z')

    print $ fromConstrB theB undefined `asTypeOf` d0

    putStrLn "Cmdargs"
    print =<< withArgs ["-a=4", "-b=False" ] (cmdArgs e0)

    -- drop the tic and z fields (which cmdargs doesn't handle)
    let dRec = d0 & from hListRecord %~ (hInit . hInit)
    print =<< withArgs ["-x=4", "-y=False"] (cmdArgs dRec)
