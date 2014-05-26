{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-} -- for VariantToRec pretty printing only
{-# LANGUAGE QuasiQuotes #-} -- for pun
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
vProto = toVariant [pun|right left up|]
  where
  left = 'a'
  right = 2 :: Int
  up = 2.3 :: Double

v = mkVariant left 'x' vProto

main = do
    putStrLn "Lookup and set"
    inspectV

    putStrLn "\nrepeat with Labelable"
    inspectV_

    putStrLn "\nSetting the missing tag does nothing:"
    print $ applyAB VariantToRec $ set right_ () v
    Left 'x' <- set _Right () (Left 'x') -- prisms for Either do the same thing
                & return

    putStrLn "\nLenses compose:"
    Nothing  <- v3 ^? up_.up_ & return
    Just 'x' <- v3 ^? left_ & return
    v4 ^? left_.left_ & print

    putStrLn "\nInternal Structure:"
    putStrLn $ indent $ show $ Record $ hMap (HFmap VariantToRec) vs

Record vs = [pun| v v2 v2' v3 v4 |]

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


-- or with the "better" label
v2' = set left_ () v


v3 = v & up_ .~ v & up_.up_ .~ "upup"
v4 = v & left_ .~ v & left_.left_ .~ "leftleft"


data VariantToRec = VariantToRec

-- | recursively change "Variant a" to "Record a" for a better show
-- instance
instance (HMapCxt (HFmap (HFmap VariantToRec)) x y, r ~ Record y) 
    => ApplyAB VariantToRec (Variant x) r where
    applyAB _ x = case hMapV VariantToRec x of
                    Variant x -> Record x

-- fallback case
instance (x ~ y) => ApplyAB VariantToRec x y where
    applyAB _ x = x


-- start a newline after every } to make the results readable
indent :: String -> String
indent xs = indent' 0 xs

indent' n xs | n < 0 = indent' 0 xs
indent' n ('}' : xs) = "}\n" ++ replicate n ' ' ++ indent' (n-4) xs
indent' n ('{' : xs) = '{' : indent' (n+4) xs
indent' n (x : xs) = x : indent' n xs
indent' _ [] = []
