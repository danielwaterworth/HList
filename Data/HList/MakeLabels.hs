{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, EmptyDataDecls #-}

{- | Slight help making value-level labels in the style of "Data.HList.Label6",
without needing to import that module.

 The following TH declaration splice should be placed at top-level, before the
 created values are used. Enable @-XTemplateHaskell@ too.

>  makeLabels ["getX","getY","draw","X"]

should expand into the following declarations

> getX = Label :: Label "getX"
> getY = Label :: Label "getY"
> draw = Label :: Label "draw"
> x    = Label :: Label "X"

plus a number of instances which replicate the single undecidable instance
in "Data.HList.Label6" which look like

> instance ShowLabel "getX" where showLabel = \_ -> "getX"
> instance ShowLabel "getY" where showLabel = \_ -> "getY"
> instance ShowLabel "draw" where showLabel = \_ -> "draw"

-}

module Data.HList.MakeLabels (makeLabels,makeLabel) where

import Data.HList.FakePrelude
import Data.HList.Record
import Language.Haskell.TH
import Data.Char
import Control.Monad

make_dname (x:xs) = mkName (toLower x : xs)

dcl n = let
    ty = litT (strTyLit n)
    labelDec = valD
                  (varP (make_dname n))
                  (normalB [| Label :: Label $ty |]) []

    showLabelInst = instanceD
            (return [])
            [t| ShowLabel $ty |]
            [valD (varP 'showLabel)
                (normalB [| \_ -> n |])
                [] ]

 in liftM2 (\a b -> [a,b])
        labelDec
        showLabelInst


-- | Our main function
makeLabels :: [String] -> Q [Dec]
makeLabels = fmap concat . mapM dcl

-- | Make a single label
makeLabel :: String -> Q [Dec]
makeLabel s = makeLabels [s]
