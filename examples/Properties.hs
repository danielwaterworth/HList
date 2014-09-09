{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- ghc-7.8 has no Typeable (x :: Symbol), so use OldTypeable
{-# LANGUAGE QuasiQuotes #-}
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
import Control.Applicative
import Test.QuickCheck
import Test.Hspec
import Language.Haskell.TH
import Control.Monad
import Data.Maybe
import GHC.TypeLits
import Control.Lens
import Control.Monad.Identity
import Data.Monoid

import Data.Array.Unboxed
import Data.HList.Variant

import Data.List
import Data.HList.HSort (hMSortBy)

instance Arbitrary a => Arbitrary (Tagged t a) where
    arbitrary = fmap Tagged arbitrary

instance Arbitrary (HList '[]) where
    arbitrary = return HNil

instance (Arbitrary x, Arbitrary (HList xs)) => Arbitrary (HList (x ': xs)) where
    arbitrary = do
      x <- arbitrary
      xs <- arbitrary
      return $ x `HCons` xs

instance (Arbitrary (Maybe x), Arbitrary (Variant (Tagged t y ': ys)),
          HExtend (Tagged s (Maybe x)) (Variant (Tagged t y ': ys)))
        => Arbitrary (Variant (Tagged s x ': Tagged t y ': ys)) where
    arbitrary = do
      x :: Maybe x <- arbitrary
      yys :: Variant (Tagged t y ': ys) <- arbitrary
      return $ Tagged x .*. yys

instance Arbitrary z => Arbitrary (Variant '[Tagged t z]) where
    arbitrary = do
      z <- arbitrary
      return $ mkVariant1 Label z

-- | This type is used to make unique types with two members.
--
-- > (BoolN True :: BoolN "x") /= (BoolN True :: BoolN "y")
--
-- is a type error
newtype BoolN (n :: Symbol) = BoolN Bool
  deriving (Eq,CoArbitrary,Arbitrary,Show,Read)

boolN next = simple $ iso (\(BoolN x) -> x) BoolN next

instance Monoid (BoolN n) where
    mempty = BoolN (getAll mempty)
    mappend (BoolN x) (BoolN y) = BoolN (getAll (mappend (All x) (All y)))

instance Arbitrary (Identity (BoolN n)) where
    arbitrary = fmap return arbitrary

lx = Label :: Label "x"
ly = Label :: Label "y"
lz = Label :: Label "z"

main = hspec $ do
 hl0
 $(let

  toN :: Int -> ExpQ
  toN n = foldr appE [| hZero |] (replicate n [| hSucc |])

  hlN :: Int -> ExpQ
  hlN n = [| \proxy -> hSequence
                $ hReplicate $(toN n)
                      (arbitrary `asTypeOf` return proxy) |]

  -- > $(rN n) (undefined :: t) :: Arbitrary t => Gen (Record ts)
  --
  -- where ts ~ '[Tagged 1 t, Tagged 2 t, Tagged 3 t, ... , Tagged n t]
  rN :: Int -> ExpQ
  rN n = [| \proxy -> do
          hl <- $(hlN n) proxy
          return $ (unlabeled # hl) `asTypeOf` $sig |]
      where sig = [| undefined |] `sigE` quantify [t| (Record :: [*] -> *) $ns |]
            quantify ty = forallT [ PlainTV (mkName ("x" ++ show i)) | i <- [1 .. n]] (return []) ty
            ns = foldr (\a b -> [t| $a ': $b |])
                    promotedNilT
                    [ [t| Tagged $(litT (numTyLit i)) $(varT (mkName ("x"++show i))) |] | i <- [1 .. fromIntegral n] ]

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

    it "hSplitAt" $
        property $ do
          hl <- genHL True
          let n = hLength hl
              l = hList2List hl
          -- hList2List doesn't like empty lists, and hMapOut id needs
          -- annotations, so the following cases are easier to construct
          -- than a direct comparison with splitAt
          return $ conjoin
            [ case hSplitAt hZero hl of
                (hNil, hl') -> (hNil `eq` HNil) .&&. (hl' `eq` hl),
              case hSplitAt n hl of
                (hl', hNil) -> (hNil `eq` HNil) .&&. (hl' `eq` hl),

              hMap (HSplitAtAppend hl) (hIterate (hSucc n) HSuccF hZero) `eq` hReplicate (hSucc n) hl ,
              map (\n -> uncurry (++) $ splitAt n l) [0 .. length l]      === replicate (length l+1) l
                -- the equivalent list-version
             ]

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

    it "hInit == tail on reverse" $
        property $ do
          let hInitReference xs = hReverse (hTail (hReverse xs))
          hl <- genHL True
          return $ hInit hl `eq` hInitReference hl

    it "hList2List/list2HList" $ property $ do
        x <- genHL True
        return $ list2HList (hList2List x) === Just x

    it "hMap equals map" $ property $ do
        f  <- arbitrary
        hl <- genHL True
        return $ hList2List (hMap f hl) `eq` map (f :: Bool -> BoolN "f") (hList2List hl)

    it "hZip" $ property $ do
        x <- genHL (BoolN True :: BoolN "x")
        y <- genHL (BoolN True :: BoolN "y")
        return $ hList2List (hZip x y) `eq` hList2List x `zip` hList2List y

    it "hZip/hUnZip" $ property $ do
        x <- genHL (BoolN True :: BoolN "x")
        y <- genHL (BoolN True :: BoolN "y")
        return $ hUnzip (hZip x y) == (x,y)

    it "hUnzip/hZip" $ property $ do
        xy <- genHL (BoolN True :: BoolN "x", BoolN True :: BoolN "y")
        let (x,y) = hUnzip xy
        return $ xy `eq` hZip x y

    it "monoid assoc" $
      property $ do
        x <- genHL (BoolN True :: BoolN "x")
        y <- genHL (BoolN True :: BoolN "x")
        z <- genHL (BoolN True :: BoolN "x")
        return $ ((x `mappend` y) `mappend` z) `eq` (x `mappend` (y `mappend` z))

    it "monoid unit" $
      property $ do
        x <- genHL (BoolN True :: BoolN "x")
        return $ conjoin
          [ x === (x `mappend` mempty),
            x === (mempty `mappend` x) ]

    it "hSort (the labels)" $ property $ do
      x <- $(rN n1) True
      let rx = x & from hListRecord %~ hReverse
      -- rN generates a record that has labels in ascending order already
      return $ conjoin [
           x `eq` (x  & from hListRecord %~ hSort),
           x `eq` (rx & from hListRecord %~ hSort),
           x `eq` (x  & from hListRecord %~ hMSortBy (Proxy :: Proxy HLeFn)),
           x `eq` (rx & from hListRecord %~ hMSortBy (Proxy :: Proxy HLeFn))
           ]

    it "hRenameLabel" $ property $ do
      r <- $(rN n1) True
      return $ conjoin
          $(listE [ [| hRenameLabel $ln lx r .!. lx === r .!. $ln |]
                | i <- [1 .. n1],
                  let ln = [| Label :: Label $(litT (numTyLit (fromIntegral i))) |]
              ])
    it "rearranged / hMapR" $ property $ do
      r <- $(rN n1) True
      let revR = r & from hListRecord %~ hReverse
          asT :: x -> As x
          asT _ = id
      -- hMap works on the reversed list
      return $ hMapR not r === (r & rearranged' . asT revR . unlabeled %~ hMap not)

   |]

  hl2 n1 n2 = [| do
    it "hAppend equals ++" $
      property $ do
        x <- $(hlN n1) True
        y <- $(hlN n2) True
        return $ hList2List (hAppend x y) === hList2List x ++ hList2List y

    it "hTranspose involution" $ property $ do
      x <- return (error "hTranspose involution") `asTypeOf` $(hlN n1) True
      xx <- $(hlN n2) x
      return $ hTranspose (hTranspose xx) === xx

    it "leftUnion / unionSR" $
      property $ do
        x <- $(rN n1) True
        y <- $(rN n2) True
        let asL r = r ^. unlabeled . to hList2List
            asLs (r1,r2) = (asL r1, asL r2)
            merge xs ys = xs ++ drop (length xs) ys
            mergeSym xs ys = (merge xs ys, merge ys xs)
        return $ conjoin [
          asL (x .<++. y) === asL x `merge` asL y,
          (x .<++. x) === x,
          (y .<++. y) === y,
          asLs (unionSR x y) === mergeSym (asL x) (asL y),
          (x `unionSR` x) === (x,x),
          (y `unionSR` y) === (y,y)]

      |]

  hl3 n1 n2 n3 = [| do
    it "hAppend/hAppendList assoc" $
      property $ do
        x <- $(hlN n1) (BoolN True :: BoolN "x")
        y <- $(hlN n2) (BoolN True :: BoolN "y")
        z <- $(hlN n3) (BoolN True :: BoolN "z")
        return $ conjoin
          [ ((x `hAppend` y) `hAppend` z) === (x `hAppend` (y `hAppend` z)),
            ((x `hAppendList` y) `hAppendList` z) === (x `hAppendList` (y `hAppendList` z))
          ]
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

  it "toLabel 1" $
    case hCast (toLabel (hLens' lx)) of
      l -> True `const` (l `asTypeOf` Just lx)

  it "toLabel 2" $ case hCast (toLabel lx) of
      l -> True `const` (l `asTypeOf` Just lx)

  it "HCast is ==" $ property $ do
    x :: BoolN "x" <- arbitrary
    x' :: BoolN "x" <- arbitrary
    return $ (x == x') ==> (x `eq` x')

  it "HCast neq" $ property $ do
    x :: BoolN "x" <- arbitrary
    y :: BoolN "y" <- arbitrary
    return (expectFailure $ x `eq` y)

  let mkXYvariant = do
        (x :: Bool) <- arbitrary
        (my :: Maybe Bool) <- arbitrary
        return $ (ly .=. my .*. mkVariant1 lx x,
                  my)

  it "variant lookup/extend" $ do
    property $ do
      (v, my) <- mkXYvariant
      return $ conjoin [
          v .!. ly == my,
          v ^? hLens' ly == my,
          v ^? hPrism ly == my ]

  it "variant update" $ property $ do
    x :: Maybe (BoolN "x") <- arbitrary
    x' :: BoolN "x'" <- arbitrary
    y :: BoolN "y" <- arbitrary
    let v = lx .=. x .*. mkVariant1 ly y
        v' | isJust x = lx .=. Just x' .*. mkVariant1 ly y
           | otherwise = lx .=. Nothing .*. mkVariant1 ly y
    return $ conjoin [
        hUpdateAtLabel lx x' v === v',
        (v & hLens' lx .~ x') === v',
        (v & hPrism lx .~ x') === v']


  it "unvariant" $ do
    property $ do
      (v, _) <- mkXYvariant
      return $ unvariant v == fromJust (msum [v .!. ly, v .!. lx])

  it "unvarianted" $ property $ do
    x :: Maybe Bool <- arbitrary
    y :: Bool <- arbitrary
    let v = lx .=. x .*. mkVariant1 ly y
        vUnitExpected = lx .=. (() <$ x) .*. mkVariant1 ly ()
        vUnit = v & unvarianted .~ ()
        vNot = lx .=. (not <$> x) .*. mkVariant1 ly (not y)
    return $ conjoin [
        v ^. unvarianted === fromMaybe y x,
        (v & unvarianted %~ not) === vNot,
        vUnit === vUnitExpected,
        vUnit ^. unvarianted === ()  ]


  it "zipVariant" $ property $ do
    x1 :: Maybe (BoolN "x1") <- arbitrary
    x2 :: Maybe (BoolN "x2") <- arbitrary
    y1 :: BoolN "y1" <- arbitrary
    y2 :: BoolN "y2" <- arbitrary

    let v1 = lx .=. x1 .*. mkVariant1 ly y1
        v2 = lx .=. x2 .*. mkVariant1 ly y2

        vrT = Proxy :: Proxy '[ Tagged "x" (BoolN "x1", BoolN "x2"),
                                Tagged "y" (BoolN "y1", BoolN "y2") ]
        vr = case (x1,x2) of
               (Just a, Just b) -> Just $ mkVariant lx (a,b) vrT
               (Nothing, Nothing) -> Just $ mkVariant ly (y1,y2) vrT
               _ -> Nothing

    return $ zipVariant v1 v2 `eq` vr

  it "variant Eq" $ property $ do
    x1 :: Maybe (BoolN "x") <- arbitrary
    x2 :: Maybe (BoolN "x") <- arbitrary
    y1 :: BoolN "y" <- arbitrary
    y2 :: BoolN "y" <- arbitrary

    let v1 = lx .=. x1 .*. mkVariant1 ly y1
        v2 = lx .=. x2 .*. mkVariant1 ly y2
    return $ (v1 == v2) === (x1 == x2 && (isJust x1 || y1 == y2))


  it "projectVariant" $ property $ do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    z <- arbitrary

    let vFull :: Variant [Tagged "a" (BoolN "a"),
                          Tagged "b" (BoolN "b"),
                          Tagged "c" (BoolN "c"),
                          Tagged "z" (BoolN "z")]
        vFull = a .*. b .*. c .*. mkVariant1 Label z

        isN x = isNothing (untag x)
        vZJ :: Variant '[Tagged "z" (BoolN "z")]
        vZJ = mkVariant1 Label z

        vZ | isN a, isN b, isN c = Just vZJ
           | otherwise = Nothing

    return $ conjoin [
       vFull ^? projected === vZ,
       -- XXX maybe projected can be made to work instead of projected'
       ((projected' # vZJ) `asTypeOf` vFull) ^? projected === Just vZJ
       ]

  it "variant/tic extend" $ do
    property $ do
      x :: BoolN "x" <- arbitrary
      my :: Maybe (BoolN "y") <- arbitrary
      let v = ly .=. my .*. mkVariant1 lx x
          tic1 = my .*. mkTIC1 x
      return $ conjoin
        [ -- v == tic1 ^. from typeIndexed,
          v^. typeIndexed == tic1
          ]



  it "variant/typeIndexed" $ do
    property $ do
      x :: BoolN "x" <- arbitrary
      y :: Maybe (BoolN "y") <- arbitrary
      let v = ly .=. y .*. mkVariant1 lx x
      let tic = v ^. typeIndexed
      return $ conjoin
        [ v .!. ly === hOccurs tic,
          v .!. ly === tic ^? ticPrism,
          v .!. ly `eq` tic .!. (Label :: Label (BoolN "y")),

          v .!. lx === hOccurs tic,
          v .!. lx === tic ^? ticPrism,
          v .!. lx `eq` tic .!. (Label :: Label (BoolN "x"))
        ]

  it "Record/typeIndexed" $ do
    property $ do
      x :: BoolN "x" <- arbitrary
      y :: BoolN "y" <- arbitrary
      let r = ly .=. y .*. lx .=. x .*. emptyRecord
          tip = r ^. typeIndexed

          asX :: As (BoolN "x")
          asX = id
          asY :: As (BoolN "y")
          asY = id

      return $ conjoin
        [ r .!. lx === hOccurs tip,
          r .!. lx === tip ^. tipyLens,
          r .!. lx `eq` tip .!. (Label :: Label (BoolN "x")),

          -- two ways to apply 'not' to the 'x' field
          (r & hLens lx . boolN %~ not) `eq`
              (r & typeIndexed %~ ttip (asX . boolN %~ not)),

          -- and repeat everything for the other field
          r .!. ly === hOccurs tip,
          r .!. ly === tip ^. tipyLens,
          r .!. ly `eq` tip .!. (Label :: Label (BoolN "y")),
          (r & hLens ly . boolN %~ not) `eq`
              (r & typeIndexed %~ ttip (asY . boolN %~ not))
        ]

  -- other operations union, projection etc.
  it "Record lookup mixing labels" $ do
    property $ do
      v1 :: BoolN "v1" <- arbitrary
      v2 :: BoolN "v1" <- arbitrary
      let l1 = Label :: Label ()
          l2 = Label :: Label 2
          r = l1 .=. v1 .*. l2 .=. v2 .*. emptyRecord
      return $ conjoin
        [ r.!.l1 `eq` v1,
          r.!.l2 `eq` v2 ]

  it "HOccurs HList" $ do
    property $ do
      x <- arbitrary
      return $ hOccurs (hEnd (hBuild x)) == (x :: Bool)

  it "HOccurs TIP" $ do
    property $ do
      x <- arbitrary
      return $ hOccurs (hEnd (hBuild x) ^. from tipHList) == (x :: Bool)

  it "HOccurs TIP inference" $
    hOccurs (HCons True HNil^. from tipHList)
      `eq` True

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

  it "unboxed" $ do
    property $ do
      (x :: Bool) <- arbitrary
      (y :: Bool) <- arbitrary
      (z :: Bool) <- arbitrary
      let r = [pun| x y z |]
          ru = r ^. unboxed
      return $ conjoin
       [ ru .!. lx === x,
         ru .!. ly === y,
         ru .!. lz === z,
         hUpdateMany r ru === ru,
         hMapRU not ru ^. from unboxed . unlabeled . re listAsHList'
                === map not [x,y,z],
         r === ru ^. from unboxed ]

  it "unboxedS" $ do
    property $ do
      (x :: Bool) <- arbitrary
      (y :: Int) <- arbitrary
      (z :: Int) <- arbitrary
      let r = [pun| x y z |]
          ru = r ^. unboxedS
      return $ conjoin
        [ ru .!. lx === x,
          ru .!. ly === y,
          ru .!. lz === z,
          r === ru ^. from unboxedS ]

  it "monoid0" $ do
    mempty `shouldBe` HNil
    mempty `shouldBe` emptyRecord
    mempty `shouldBe` emptyTIP


  it "identity: rearranged relabeled unlabeled" $ do
    let r = lx .=. True .*.
            ly .=. () .*. emptyRecord
    (r ^. rearranged) `shouldBe` r
    (r ^. relabeled) `shouldBe` r
    (r & unlabeled %~ id) `shouldBe` r

  it "rearranged" $ do
    let r = lx .=. True .*.
            ly .=. () .*. emptyRecord
    let r2 = ly .=. () .*.
            lx .=. True .*. emptyRecord
    (r ^. rearranged) `shouldBe` r2

  it "relabeled" $ do
    let r = lx .=. True .*.
            ly .=. () .*. emptyRecord
    let r2 = ly .=. True .*.
            lx .=. () .*. emptyRecord
    (r ^. relabeled) `shouldBe` r2

  it "hMaybied" $ property $ do
    mx :: Maybe Bool <- arbitrary
    my :: Maybe Bool <- arbitrary
    let r = lx .=. mx .*. ly .=. my .*. emptyRecord
        vT = Proxy :: Proxy [Tagged "x" Bool, Tagged "y" Bool]
        (val, v)  = case (mx,my) of
          (Just x, Nothing) -> (Just x, Just (mkVariant lx x vT))
          (Nothing, Just y) -> (Just y, Just (mkVariant ly y vT))
          _ -> (Nothing, Nothing)

    return $ conjoin [
      (r^?hMaybied <&> unvariant) `eq` val,
      isJust v ==> ( hMaybied' # fromJust v === r ) ]

  it "hMaybied 2" $ property $ do
    x :: BoolN "x"  <- arbitrary
    my :: Maybe (BoolN "y") <- arbitrary
    let v = ly .=. my .*. mkVariant1 lx x
        r = ly .=. my .*. lx .=. Just x .*. emptyRecord

    return $ (r ^? hMaybied) `eq` do
      guard $ isNothing my
      Just v

  it "hPrism" $ property $ do
    x :: Bool <- arbitrary
    my :: Maybe (Maybe ()) <- arbitrary
    let v  = ly .=. my .*. mkVariant1 lx x
        v' = ly .=. my .*. mkVariant1 lx (not x)

        tic = my .*. mkTIC1 x
        tic' = my .*. mkTIC1 (not x)

    return $ conjoin
      [ v' `eq` (v & hPrism lx %~ not),
        tic' `eq` (tic & hLens' (Label :: Label Bool) %~ not),
        -- should work, but it doesn't
        -- tic' `eq` (tic & hLens' Label %~ not),
        tic' `eq` (tic & ticPrism %~ not)
      ]


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



-- | A more general type than @===@ used to
-- ensure that both sides can infer the same type
eq :: (Show a, Show b, HCast a b, HCast b a, Eq a, Eq b) => a -> b -> Property
eq x y = hCast x === Just y .&&. Just x === hCast y
infix 4 `eq`


data HSuccF = HSuccF

instance (psn ~ Proxy (HSucc n),
        pn ~ Proxy n) => ApplyAB HSuccF pn psn where
    applyAB _ = hSucc


data HSplitAtAppend l = HSplitAtAppend (HList l)
instance (pn ~ Proxy n,
          HSplitAt n l a b,
          HAppend (HList a) (HList b),
          y ~ HAppendR (HList a) (HList b)) => ApplyAB (HSplitAtAppend l) pn y where
    applyAB (HSplitAtAppend l) n = case hSplitAt n l of
                                     (a,b) -> hAppend a b
