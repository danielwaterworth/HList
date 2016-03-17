{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Properties.KW where


import Properties.Common
import Test.QuickCheck
import Data.HList.CommonMain
import Test.Hspec


kwSpecs = describe "kw" $ do
    it "f1" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      let f2 (Label :: Label "x") x () = f1 x
          f = f2 .*. recToKW [pun| x |]
      return $ conjoin
        [ kw f lx x2 () `eq` f1 x2,
          kw f () `eq` f1 x ]

    -- a function of two arguments can be made into a keyword function
    it "f2" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y" -> BoolN "z") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      y :: BoolN "y" <- arbitrary
      y2 :: BoolN "y" <- arbitrary

      let f2 (_ :: Label "x") x (_ :: Label "y") y () = f1 x y
          f = f2 .*. recToKW [pun| x y |]

      return $ conjoin
        [ kw f lx x2 ly y2 () `eq` f1 x2 y2,
          kw f ly y2 lx x2 () `eq` f1 x2 y2,
          kw f ly y2 () `eq` f1 x y2,
          kw f lx x2 () `eq` f1 x2 y,
          kw f () `eq` f1 x y ]

    -- alternatively, a function taking a record is pretty much
    -- a keyword argument. Error messages for missing keywords
    -- are a bit worse (blame hRearrange')
    it "f2Alt" $ property $ do
      (f1 :: BoolN "x" -> BoolN "y" -> BoolN "z") <- arbitrary
      x :: BoolN "x" <- arbitrary
      x2 :: BoolN "x" <- arbitrary
      y :: BoolN "y" <- arbitrary
      y2 :: BoolN "y" <- arbitrary

      let addDef new = hRearrange (Proxy :: Proxy [Label "x", Label "y"]) (new .<++. [pun| x y |])
          f2 (addDef  -> [pun| (x y) |]) = f1 x y
      return $ conjoin
        [ f2 emptyRecord `eq` f1 x y,
          f2 (lx .=. x2 .*. emptyRecord) `eq` f1 x2 y,
          f2 (ly .=. y2 .*. emptyRecord) `eq` f1 x y2,
          f2 (lx .=. x2 .*. ly .=. y2 .*. emptyRecord) `eq` f1 x2 y2,
          f2 (ly .=. y2 .*. lx .=. x2 .*. emptyRecord) `eq` f1 x2 y2
        ]

