{-# LANGUAGE TemplateHaskell #-}

{- | Description : Automate some of the ways to make labels.

-}

module Data.HList.MakeLabels (
    makeLabels,
    makeLabels3,
    makeLabels6,
    -- | see also 'Data.HList.Labelable.makeLabelable'
    ) where

import Data.HList.FakePrelude
import Data.HList.Label3

import Language.Haskell.TH
import Data.Char
import Control.Monad

make_cname, make_dname :: String -> Name
make_cname (x:xs) = mkName ("Label" ++ toUpper x : xs)
make_cname _ = error "Data.HList.MakeLabels.make_cname: empty string"

make_dname (x:xs) = mkName (toLower x : xs)
make_dname _ = error "Data.HList.MakeLabels.make_dname: empty string"

dcl :: String -> Q [Dec]
dcl n = let
    c = make_cname n
    d = make_dname n

    dd = dataD (return []) c [] [] [{- 'Typeable -}]

    labelSig = sigD d [t| Label $(conT c) |]

    labelDec = valD
                  (varP d)
                  (normalB [| Label |])
                  []

    showLabelInst = instanceD
            (return [])
            [t| ShowLabel $(conT c) |]
            [valD (varP 'showLabel)
                (normalB [| \_ -> n |])
                [] ]

    showInst = instanceD
            (return [])
            [t| Show $(conT c) |]
            [valD (varP 'show)
                (normalB [| \_ -> n |])
                [] ]

 in sequence [
        labelSig,
        labelDec,

        dd,

        showLabelInst,
        showInst ]


{- |

Labels like "Data.HList.Label4" used to provide (only no Typeable).

 The following TH declaration splice should be placed at top-level, before the
 created values are used. Enable @-XTemplateHaskell@ too.

>  makeLabels ["getX","getY","draw","X"]

should expand into the following declarations

> data LabelGetX
> data LabelGetY
> data LabelDraw
> data LabelX

> getX = Label :: Label LabelGetX
> getY = Label :: Label LabelGetY
> draw = Label :: Label LabelDraw
> x    = Label :: Label LabelX


> instance ShowLabel LabelGetX where showLabel = \_ -> "getX"
> instance ShowLabel LabelGetY where showLabel = \_ -> "getY"
> instance ShowLabel LabelDraw where showLabel = \_ -> "draw"

-}
makeLabels :: [String] -> Q [Dec]
makeLabels = fmap concat . mapM dcl


-- | for "Data.HList.Label3"
makeLabels3 :: String -- ^ namespace
    -> [String] -- ^ labels
    -> Q [Dec]
makeLabels3 ns (k:ks) =
    let pt1 = fmap (concatMap (drop 2)) $ mapM dcl (ns : k : ks)

        sq1 = valD (varP (make_dname k))
                (normalB [| firstLabel (undefined :: $(conT (make_cname ns)))
                                       (undefined :: $(conT (make_cname k))) |])
                []

        sqs = [ valD (varP (make_dname k2))
                (normalB [| nextLabel $(varE (make_dname k1))
                                    (undefined :: $(conT (make_cname k2))) |])
                []

                | (k1,k2) <- zip (k:ks) ks ]

    in fmap concat $ sequence [ pt1, sequence (sq1 : sqs) ]
-- possibly there is a better option
makeLabels3 ns [] = fail ("makeLabels3 "++ ns ++ " []")

-- | for "Data.HList.Label6"
makeLabels6 :: [String] -> Q [Dec]
makeLabels6 ns = fmap concat $ forM ns $ \n -> sequence
  [sigD (make_dname n) [t| Label $(litT (strTyLit n)) |],
   valD (varP (make_dname n)) (normalB [| Label |]) []]
