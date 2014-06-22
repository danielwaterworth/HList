{-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds #-}
{- | Demonstrates @hLens@. See also labelable.hs which is more "convenient"

-}
module Main where
import Data.HList.CommonMain
import Control.Lens

makeLabels6 (words "x y z")

r = x .=. "hi" .*.
    y .=. (y .=. 321 .*. x .=. 123 .*. emptyRecord) .*.
    emptyRecord

rSmall = x .=. "" .*. emptyRecord

x' a = hLens x a
y' a = hLens y a

main = do
    print (view (hLens x) r)
    print (set (hLens x) () r)

    print (r ^. hLens y . hLens x)
    print (r & hLens y . hLens y .~ "xy")


    putStrLn "\n\nand repeat:"

    -- and now for with hLens applied second
    print (view x' r)
    print (set x' () r)

    print (r ^. y' . y')
    print (r & y' . y' .~ "xy")

    putStrLn "\n\nIsos"
    print (r & unlabeled . hTuple . _1 .~ ())
    print (r & unlabeled . hTuple . _2 .~ ())

    print (z .=. () .*. r
              & unlabeled . from tipHList %~ ttip (\x z -> x ++ show (z :: ())))

    r ^! unlabeled . from tipHList . act tipPutStrLn


tipPutStrLn tip = ttipM ?? tip $ \x -> do
  putStrLn x
  return x
