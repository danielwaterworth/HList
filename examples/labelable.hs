{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell, DataKinds, PolyKinds,
  GADTs, ConstraintKinds #-}
{- | Demonstrates @hLens'@

may be worthwhile to have a lens-free test suite, doing stuff like:

> case x (Identity  . (++"there")) r of Identity t -> t

-}
module Main where
import Data.HList.CommonMain
import Control.Lens

import Text.Read

makeLabelable "x y"

#if __GLASGOW_HASKELL__ < 707
#define INT_SIG_76 :: Int
#else
#define INT_SIG_76
#endif

r = x .==. "hi" .*.
    y .==. (y .==. 321 .*. x .==. 123 .*. emptyRecord) .*.
    emptyRecord

main = do
    print (r ^. x)
    print (r & x .~ ())

    -- ghc-7.6 doesn't default when r is involved lower down,
    -- while 7.8.2 does
    print (r ^. y . y  INT_SIG_76)
    print (r ^. y . x  INT_SIG_76)

    print (r & y . y .~ "xy")

    putStrLn "\nread-show"
    print (readMaybe (show r) `asTypeOf` Just r)
    print (readMaybe "Record{x=\"hi\",y=Record{y=321,x=123}}" `asTypeOf` Just r)

    -- there is no permuting of labels
    print (readMaybe "Record{y=Record{y=321,x=123},x=\"hi\"}" `asTypeOf` Just r)

    print $ (r ^. rearranged) `asTypeOf` (undefined :: Record '[Tagged "y" t, Tagged "x" s])
