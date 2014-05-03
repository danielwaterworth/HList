{-# LANGUAGE FlexibleContexts, TemplateHaskell, DataKinds, PolyKinds #-}
{- | Demonstrates @hLens'@

may be worthwhile to have a lens-free test suite, doing stuff like:

> case x (Identity  . (++"there")) r of Identity t -> t

-}
module Main where
import Data.HList.CommonMain
import Control.Lens

import Text.Read

makeLabelable "x y"

r = x .==. "hi" .*.
    y .==. (y .==. 321 .*. x .==. 123 .*. emptyRecord) .*.
    emptyRecord

main = do
    print (r ^. x)
    print (r & x .~ ())

    print (r ^. y . y)
    print (r & y . y .~ "xy")

    putStrLn "\nread-show"
    print (readMaybe (show r) `asTypeOf` Just r)
    print (readMaybe "Record{x=\"hi\",y=Record{y=321,x=123}}" `asTypeOf` Just r)

    -- there is no permuting of labels
    print (readMaybe "Record{y=Record{y=321,x=123},x=\"hi\"}" `asTypeOf` Just r)
