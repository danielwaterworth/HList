{-# LANGUAGE FlexibleContexts, TemplateHaskell, DataKinds, PolyKinds #-}
{- | Demonstrates @hLens'@

-}
module Main where
import Data.HList.CommonMain
import Control.Lens

makeLabelable "x y"

r = x .==. "hi" .*.
    y .==. (y .==. 321 .*. x .==. 123 .*. emptyRecord) .*.
    emptyRecord

main = do
    print (r ^. x)
    print (r & x .~ ())

    print (r ^. y . y)
    print (r & y . y .~ "xy")
