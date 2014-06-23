{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Description: quickcheck tests
--
-- Many of the tests here use quickcheck. The lengths of the hlists
-- involved exhaustively cover a small range, while the elements
-- are random samples.
module Main where

import Data.HList.CommonMain
import Test.QuickCheck
import Test.Hspec
import Language.Haskell.TH
import Control.Monad
import Data.Maybe
import GHC.TypeLits
import Control.Lens
import Data.Typeable
import Control.Monad.Identity


instance Arbitrary (HList '[]) where
    arbitrary = return HNil

instance (Arbitrary x, Arbitrary (HList xs)) => Arbitrary (HList (x ': xs)) where
    arbitrary = do
      x <- arbitrary
      xs <- arbitrary
      return $ x `HCons` xs

-- | This type is used to make unique types with two members.
--
-- > (BoolN True :: BoolN "x") /= (BoolN True :: BoolN "y")
--
-- is a type error
newtype BoolN (n :: Symbol) = BoolN Bool
  deriving (Eq,CoArbitrary,Arbitrary,Show,Read)

instance Arbitrary (Identity (BoolN n)) where
    arbitrary = fmap return arbitrary

lx = Label :: Label "x"
ly = Label :: Label "y"

main = hspec $ do
 hl0
 $(let

  toN :: Int -> ExpQ
  toN n = foldr appE [| hZero |] (replicate n [| hSucc |])

  hlN :: Int -> ExpQ
  hlN n = [| \proxy -> hSequence
                $ hReplicate $(toN n)
                      (arbitrary `asTypeOf` return proxy) |]

  -- specs for 1 HList of length >= 1
  hl1 n1 = [| do
    let -- | generate a HList of length nMax containing elements
        -- selected from there
        genHL proxy = $(hlN n1) proxy

    it "hLength/hReplicate" $
        property $ do
          xs <- genHL True
          return $ hNat2Integral (hLength xs) == hNat2Integral $(toN n1)

    it "hInits last id" $
        property $ do
          xs <- genHL True
          return $ hLast (hInits xs) == xs

    it "hInits head empty" $
        property $ do
          xs <- genHL True
          return $ hHead (hInits xs) == HNil

    it "hTails head id" $
        property $ do
          xs <- genHL True
          return $ hHead (hTails xs) == xs

    it "hTails last empty" $
        property $ do
          xs <- genHL True
          return $ hLast (hTails xs) == HNil

    it "hScanr equals scanr" $
        property $ do
          f <- arbitrary
          a <- arbitrary
          hl <- genHL True
          return $ hList2List (hScanr (BinF f) a hl)
                  == scanr f a (hList2List hl)

    it "hFoldr equals foldr" $
        property $ do
          f <- arbitrary
          a <- arbitrary
          hl <- genHL True
          return $ hFoldr (BinF f) a hl == foldr f a (hList2List hl)

    it "hFoldr1 equals foldr1" $
        property $ do
          f <- arbitrary
          hl <- genHL True
          return $ hFoldr1 (BinF f) hl == foldr1 f (hList2List hl)

    it "hFoldl equals foldl" $
        property $ do
          f <- arbitrary
          a <- arbitrary
          hl <- genHL True
          return $ hFoldl (BinF f) a hl == foldl f a (hList2List hl)

    it "hAppend empty is identity" $
        property $ do
          x <- genHL (BoolN True :: BoolN "x")
          return $ all (== x) [HNil `hAppend` x, x `hAppend` HNil]

    it "hReverse involution" $ do
        property $ do
          x <- genHL True
          return $ x == hReverse (hReverse x)

    it "hReverse does nothing for ()" $
        let xs = hReplicate $(toN n1) ()
        in xs `shouldBe` hReverse xs

    let hInitReference xs = hReverse (hTail (hReverse xs))
    it "hInit == tail on reverse" $
        property $ do
          hl <- genHL True
          return $ hInit hl == hInitReference hl

    it "hMap equals map" $ property $ do
        f  <- arbitrary
        hl <- genHL True
        return $ hList2List (hMap f hl) == map (f :: Bool -> BoolN "f") (hList2List hl)

    it "hZip" $ property $ do
        x <- genHL (BoolN True :: BoolN "x")
        y <- genHL (BoolN True :: BoolN "y")
        return $ hList2List (hZip x y) == hList2List x `zip` hList2List y

    it "hZip/hUnZip" $ property $ do
        x <- genHL (BoolN True :: BoolN "x")
        y <- genHL (BoolN True :: BoolN "y")
        return $ hUnzip (hZip x y) == (x,y)

    it "hUnzip/hZip" $ property $ do
        xy <- genHL (BoolN True :: BoolN "x", BoolN True :: BoolN "y")
        let (x,y) = hUnzip xy
        return $ xy == hZip x y
   |]

  hl2 n1 n2 = [| do
    it "equals ++" $
      property $ do
        x <- $(hlN n1) True
        y <- $(hlN n2) True
        return $ hList2List (hAppend x y) == hList2List x ++ hList2List y

    it "hTranspose involution" $ property $ do
      x <- return (error "hTranspose involution") `asTypeOf` $(hlN n1) True
      xx <- $(hlN n2) x
      return $ hTranspose (hTranspose xx) == xx

    |]

  hl3 n1 n2 n3 = [| do
    it "hAppend assoc" $
      property $ do
        x <- $(hlN n1) (BoolN True :: BoolN "x")
        y <- $(hlN n2) (BoolN True :: BoolN "y")
        z <- $(hlN n3) (BoolN True :: BoolN "z")
        return $ ((x `hAppend` y) `hAppend` z) == (x `hAppend` (y `hAppend` z))
    |]

  in doE $
        [ noBindS
               [| describe $(stringE (show n)) $(hl1 n) |]
            | n <- [1 .. 5]]
    ++ [ noBindS [| describe $(stringE (show (n1,n2))) $(hl2 n1 n2) |]
      | n1 <- [1 .. 3],
        n2 <- [1 .. 3] ]
    ++ [ noBindS [| describe $(stringE (show (n1,n2,n3))) $(hl3 n1 n2 n3) |]
      | n1 <- [0 .. 2],
        n2 <- [0 .. 1],
        n3 <- [0 .. 2],
        not $ all (==0) [n1,n2,n3] ]
  )


data BinF b = BinF (b -> b -> b)

instance (bb ~ (b, b), b ~ b') => ApplyAB (BinF b') bb b where
    applyAB (BinF f) = uncurry f


-- | tests for a fixed length
hl0 = describe "0 -- length independent"  $ do
  hTuples

  let mkXYvariant = do
        (x :: Bool) <- arbitrary
        (my :: Maybe Bool) <- arbitrary
        return $ (ly .=. my .*. mkVariant1 lx x,
                  my)

  it "variant lookup/extend" $ do
    property $ do
      (v, my) <- mkXYvariant
      return $ v .!. ly == my

  it "unvariant" $ do
    property $ do
      (v, _) <- mkXYvariant
      return $ unvariant v == fromJust (msum [v .!. ly, v .!. lx])

  it "HOccurs HList" $ do
    property $ do
      x <- arbitrary
      return $ hOccurs (hEnd (hBuild x)) == (x :: Bool)

  it "HOccurs TIP" $ do
    property $ do
      x <- arbitrary
      return $ hOccurs (hEnd (hBuild x) ^. from tipHList) == (x :: Bool)

  it "HOccurs TIP inference" $
    Just (hOccurs (HCons True HNil^. from tipHList))
      `shouldBe` cast True

  it "ttip 3" $ do
    property $ do
      f <- arbitrary
      (a :: BoolN "a") <- arbitrary
      (b :: BoolN "b") <- arbitrary
      (c :: BoolN "c") <- arbitrary
      let tp = a .*. b .*. c .*. emptyTIP
      return $ hOccurs (ttip f tp) == (f a b c :: BoolN "a")

  it "ttipM 3" $ do
    property $ do
      f <- arbitrary
      (a :: BoolN "a") <- arbitrary
      (b :: BoolN "b") <- arbitrary
      (c :: BoolN "c") <- arbitrary
      let tp = a .*. b .*. c .*. emptyTIP
      return $ hOccurs (runIdentity (ttipM f tp)) == (runIdentity (f a b c) :: BoolN "a")

  it "Show/Read instances" $ do
    show (hEnd (hBuild 1 2 3)) `shouldBe` "H[1, 2, 3]"

    let r = lx .=. 'x' .*. ly .=. "y" .*. emptyRecord
    show r `shouldBe` "Record{x='x',y=\"y\"}"
    read (show r) `shouldBe` r

    show (r ^. unlabeled . from tipHList) `shouldBe` "TIPH['x', \"y\"]"

    v <- return $ map ($ r) [mkVariant lx 'a', mkVariant ly "ly"]

    show v `shouldBe` "[V{x='a'},V{y=\"ly\"}]"
    read (show v) `shouldBe` v

    -- XXX ticVariant needs to adjust the labels
    -- show (map (^. from ticVariant) v) `shouldBe` "[TIC['a'], TIC[\"ly\"]]"

hTuples = do
  it "HTuple0" $ HNil ^. hTuple `shouldBe` ()

  it "HTuple2" $ property $ do
    a <- arbitrary
    b <- arbitrary
    let ab = (a :: BoolN "a",b :: BoolN "b")
    return $ hBuild a b ==  ab ^. from hTuple
        && hBuild a b ^. hTuple == ab

  it "HTuple3" $ property $ do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    let abc = (a :: BoolN "a",b :: BoolN "b",c :: BoolN "c")
    return $ hBuild a b c ==  abc ^. from hTuple
      && hBuild a b c ^. hTuple == abc

  it "HTuple4" $ property $ do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    let abc = (a :: BoolN "a",b :: BoolN "b",c :: BoolN "c",
               d :: BoolN "d")
    return $ hBuild a b c d ==  abc ^. from hTuple
      && hBuild a b c d ^. hTuple == abc

  it "HTuple5" $ property $ do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    let abc = (a :: BoolN "a",b :: BoolN "b",c :: BoolN "c",
               d :: BoolN "d", e :: BoolN "e")
    return $ hBuild a b c d e ==  abc ^. from hTuple
      && hBuild a b c d e ^. hTuple == abc

  it "HTuple6" $ property $ do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    let abc = (a :: BoolN "a",b :: BoolN "b",c :: BoolN "c",
               d :: BoolN "d", e :: BoolN "e", f :: BoolN "f")
    return $ hBuild a b c d e f ==  abc ^. from hTuple
      && hBuild a b c d e f ^. hTuple == abc
