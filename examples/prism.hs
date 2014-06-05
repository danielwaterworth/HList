{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-} -- for pun
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Data.HList.CommonMain
import Control.Lens

-- generate left = Label :: Label "left"
makeLabels6 (words "left right up down")

--- define the Labelable labels manually
left_ = hLens' left
right_ = hLens' right
up_ = hLens' up
down_ = hLens' down

-- this definition is needed to decide what order
-- to put the fields in, as well as their initial types
r = [pun|right left up|] where
  left = 'a'
  right = 2 :: Int
  up = 2.3 :: Double

v = mkVariant left 'x' r

main = do
    putStrLn "Lookup and set"
    inspectV

    putStrLn "\nrepeat with Labelable"
    inspectV_

    putStrLn "\nSetting the missing tag does nothing:"
    print $ set right_ () v
    Left 'x' <- set _Right () (Left 'x') -- prisms for Either do the same thing
                & return

    putStrLn "\nLenses compose:"
    Nothing  <- v3 ^? up_.up_ & return
    Just 'x' <- v3 ^? left_ & return
    v4 ^? left_.left_ & print

    putStrLn "\nLenses+prisms compose:"
    inspectRV

    putStrLn "\n\"extension\""
    extensionTests

    putStrLn "\nInternal Structure:"
    putStrLn $ indent $ show vs



lazyX = mkVariant (Label :: Label "x") 'a' lazyProto
lazyY = mkVariant (Label :: Label "y") (2.5 :: Double) lazyProto

lazyProto = undefined :: Record
  '[Tagged "x" x, Tagged "y" y]

vs = [pun| v v2 v2' v3 v4 v5 v6 v7 |]

inspectV = do
    v ^? hPrism left & print
    v ^? hPrism right & print
    v ^? hPrism up & print
    v2 ^? hPrism left & print

-- note that we can change the type of the 'x' field
-- from Char to ()
v2 = set (hPrism left) () v

inspectV_ = do
    v ^? left_ & print
    v ^? right_ & print
    v ^? up_ & print
    v2' ^? left_ & print


inspectRV = do
    r2 ^? down_.left_ & print
    r2 ^? down_.right_ & print
    r2 ^? du & print

du = down_.up_

r2 = down_ .==. v .*. r


-- or with the "better" label
v2' = set left_ () v


v3 = v & up_ .~ v & up_.up_ .~ "upup"
v4 = v & left_ .~ v & left_.left_ .~ "leftleft"


extensionTests = do
    Just "hi" <- v5 ^? down_ & return
    Just "hi" <- v6 ^? down_ & return
    Nothing   <- v7 ^? down_ & return
    Just 'x'  <- v7 ^? left_ & return
    return ()


v5 = down .=. Just "hi" .*. v
v6 = down_ .==. Just "hi" .*. v
v7 = down .=. (Nothing :: Maybe String) .*. v

-- start a newline after every } to make the results readable
indent :: String -> String
indent xs = indent' 0 xs

indent' n xs | n < 0 = indent' 0 xs
indent' n ('}' : xs) = "}\n" ++ replicate n ' ' ++ indent' (n-4) xs
indent' n ('{' : xs) = '{' : indent' (n+4) xs
indent' n (x : xs) = x : indent' n xs
indent' _ [] = []
