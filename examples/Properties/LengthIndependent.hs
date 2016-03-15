{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Properties.LengthIndependent where
import Properties.Common
import Control.Lens
import Data.HList.CommonMain
import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Monad

import Data.Generics

makeLabels3 "lengthindependent" (words "lx_ ly_")


-- | tests for a fixed length
hl0 = describe "0 -- length independent"  $ do
  hTuples

  it "listAsHList" $ property $ do
    (f :: Bool -> Bool) <- arbitrary
    let bools = [True,False]
        mapF (a `HCons` b `HCons` HNil) = f a `HCons` f b `HCons` HNil
        len3 = id :: As (HList '[a,b,c])
        len2 = id :: As (HList [a,b])
        len1 = id :: As (HList '[a])
    return $ conjoin
      [ (bools & listAsHList %~ mapF) `eq` map f bools,
        (bools & listAsHList' . len2 %~ hMap f ) `eq` map f bools,
        (bools & listAsHList' . len3 %~ hMap f ) `eq` ([] :: [Bool]),
        (bools & listAsHList' . len1 %~ hMap f ) `eq` ([] :: [Bool])]

  it "read0" $ read "H[]" `shouldBe` HNil

  it "Fun" $ property $ do
    let plusF = Fun (+1) :: Fun Num '()
    x :: Int <- arbitrary
    y :: Double <- arbitrary
    return $ hMap plusF (hBuild x y) === hEnd (hBuild (x+1) (y+1))

  it "Fun 2" $ property $ do
    let showSuccF = Fun (show . (+1)) :: Fun [Num,Show] String
    x :: Int <- arbitrary
    y :: Double <- arbitrary
    return $ hMapOut showSuccF (hBuild x y) === [ show (x+1), show (y+1)]

  it "Fun'" $ property $ do
    x :: Bool <- arbitrary
    return $ applyAB (Fun' read :: Fun' Read String) (show x) === x

  it "HComp" $ property $ do
    let f = Fun (+1) :: Fun Num '()
        g = Fun show :: Fun Show String
        gof = g `HComp` f

    x :: Int <- arbitrary
    y :: Double <- arbitrary

    let ref = [show (x+1), show (y+1)]

    return $ conjoin [
        hMapOut gof (hBuild x y) `eq` ref,
        hMapOut g (hMap f (hBuild x y)) `eq` ref ]

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

  it "HMapOutV" $ do
    property $ do
      (v, _) <- mkXYvariant
      return $ hMapOutV not v `eq` not (fromJust (msum [v .!. ly, v .!. lx]))

  it "zipVR" $ property $ do
    (f :: BoolN "x" -> BoolN "x'",
     g :: BoolN "y" -> BoolN "y'",
     b,x,y) <- arbitrary
    let p = lx .*. ly .*. emptyProxy
        v  | b = mkVariant lx x Proxy `asLabelsOf` p
           | otherwise = mkVariant ly y Proxy
        v' | b = mkVariant lx (f x) Proxy `asLabelsOf` p
           | otherwise = mkVariant ly (g y) Proxy

        fun = lx .=. f .*. ly .=. g .*. emptyRecord

    return $ zipVR fun v `eq` v'

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
      v2 :: BoolN "v2" <- arbitrary
      v3 :: BoolN "v3" <- arbitrary
      let l1 = Label :: Label ()
          l2 = Label :: Label 2
          l3 = Label :: Label "3"
          p = Proxy :: Proxy '[Label (), Label 2 , Label "3"]

          p1 = consl1 $ l2 .*. l3 .*. emptyProxy

          -- HExtend doesn't support Label5
          consl1 :: Proxy x -> Proxy (Label () ': x)
          consl1 _ = Proxy

          r = hEndR (hBuild v1 v2 v3) `asLabelsOf` p
      return $ conjoin
        [ r.!.l1 `eq` v1,
          r.!.l2 `eq` v2,
          p1 `eq` p ]

  it "Record hLookupByLabelM" $ property $ do
    v :: BoolN "v" <- arbitrary
    w :: BoolN "v" <- arbitrary
    let r = [pun| v |]
    return $ conjoin
      [ hLookupByLabelM (Label :: Label "v") r w `eq` v,
        hLookupByLabelM (Label :: Label "w") r w `eq` w ]

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

  it "tipyLens" $ property $ do
    u :: BoolN "u" <- arbitrary
    v :: BoolN "v" <- arbitrary
    w :: BoolN "w" <- arbitrary
    let r = tipHList # hBuild v w
    return $ conjoin
      [ (r & tipyLens %~ ( \ (_ :: BoolN "v") -> u)) `eq` tipHList # hBuild u w,
        (r & tipyLens %~ ( \ (_ :: BoolN "w") -> u)) `eq` tipHList # hBuild v u
        ]

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
    show (hEnd (hBuild 1 2 3)) `shouldBe` "H[1,2,3]"

    let r = lx .=. 'x' .*. ly .=. "y" .*. emptyRecord
    show r `shouldBe` "Record{x='x',y=\"y\"}"
    read (show r) `shouldBe` r

    show (r ^. unlabeled . from tipHList) `shouldBe` "TIPH['x',\"y\"]"

    v <- return $ map ($ r) [mkVariant lx 'a', mkVariant ly "ly"]

    show v `shouldBe` "[V{x='a'},V{y=\"ly\"}]"
    read (show v) `shouldBe` v

    show (map (^. typeIndexed') v) `shouldBe` "[TIC{char='a'},TIC{[Char]=\"ly\"}]"

  it "Data instances gread/gshow" $ do
    property $ do
      a :: Maybe Bool <- arbitrary
      b :: Bool <- arbitrary
      let h = hEnd $ hBuild a b
          v = lx_ .=. a .*. mkVariant1 ly_ b
          r = (unlabeled # h) `asLabelsOf` pLabel3

          -- ghc-7.8 can't use pLabel5 (due to a lack of Typeable "x")
          pLabel3 = lx_ .*. ly_ .*. emptyProxy
          pLabel5 = lx .*. ly .*. emptyProxy -- Proxy :: Proxy ["x","y"]
      return $ conjoin
        [ gread (gshow h) === [(h, "")],
          gread (gshow v) === [(v, "")],
          gread (gshow r) === [(r, "")] ]

  it "Enum" $ do
    show [ mkVariant lx False (Proxy :: Proxy '[Tagged "x" Bool, Tagged "y" Bool]) .. maxBound ]
      `shouldBe` "[V{x=False},V{x=True},V{y=False},V{y=True}]"

  it "minBound" $ do
    mkVariant lx False (Proxy :: Proxy '[Tagged "x" Bool, Tagged "y" Bool])
      `shouldBe` minBound



  it "projected" $ do
    property $ do
      (f :: Bool -> Bool -> Bool) <- arbitrary
      x :: Bool <- arbitrary
      y :: Bool <- arbitrary
      let r = lx .=. x .*. ly .=. y .*. lz .=. () .*. emptyRecord
          g1 [pun| (x y) |] = case f x y of z -> [pun| z |]
          g2 [pun| (y x) |] = case f x y of z -> [pun| z |]

          rExpect = lx .=. x .*. ly .=. y .*. lz .=. f x y .*. emptyRecord

      containX :: Bool <- arbitrary
      let
          v p | containX = mkVariant lx x p
            | otherwise = mkVariant ly y Proxy


          v1 = v (Proxy :: Proxy '[Tagged "x" Bool, Tagged "y" Bool, Tagged "z" Char])
          v2 = v (Proxy :: Proxy '[Tagged "x" Bool, Tagged "y" Bool])

          v1not = v1 & sameLength . sameLabels . projected %~ hMapV Just . (`asLabelsOf` labelsOf v2)


      return $ conjoin
        [ (r & sameLabels . projected %~ g1) `eq` rExpect
        , (r & sameLabels . projected %~ g2) `eq` rExpect
        , (v1 ^? projected) === Just v2
        , review projected v2 === v1
          ]


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

  it "sortForRecordUS" $ do
    property $ do
      a :: Bool <- arbitrary
      b :: (Bool,Bool) <- arbitrary
      c :: Bool <- arbitrary
      d :: (Bool,Bool) <- arbitrary
      let r = [pun| a b c d |]
          sr = sortForRecordUS r
          ssr = sortForRecordUS sr

      return $ conjoin
        [ sr `eq` ssr,
          sr .!. (Label :: Label "a") === a,
          sr .!. (Label :: Label "b") === b,
          sr .!. (Label :: Label "c") === c,
          sr .!. (Label :: Label "d") === d,
          hRearrange' sr === r
        ]


  it "monoid0" $ do
    mempty `shouldBe` HNil
    mempty `shouldBe` emptyRecord
    mempty `shouldBe` emptyTIP
    mempty `shouldBe` mkVariant1 lx ()
    mempty `shouldBe` (mkVariant ly () (Proxy :: Proxy '[Tagged "x" [Int], Tagged "y" ()]))


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

  it "hMaybied update" $ property $ do
    (f :: BoolN "x" -> BoolN "w",y :: BoolN "y") <- arbitrary
    let x  = lx .=. (Nothing :: Maybe (BoolN "x")) .*. ly .=. Just y .*. emptyRecord
        x' = lx .=. (Nothing :: Maybe (BoolN "w")) .*. ly .=. Just y .*. emptyRecord

    return $ (x & sameLength . hMaybied . hPrism lx %~ f) === x'

  it "hPrism" $ property $ do
    x :: Bool <- arbitrary
    my :: Maybe (Maybe ()) <- arbitrary
    let v  = ly .=. my .*. mkVariant1 lx x
        v' = ly .=. my .*. mkVariant1 lx (not x)

        tic = my .*. mkTIC1 x
        tic' = my .*. mkTIC1 (not x)

    return $ conjoin
      [ v' `eq` (v & hPrism lx %~ not),
        tic' `eq` (tic & hLens' Label %~ not),
        tic' `eq` (tic & ticPrism %~ not)
      ]

  it "hDeleteAtLabel" $ property $ do
    vx :: BoolN "x" <- arbitrary
    vy :: BoolN "y" <- arbitrary
    vz :: BoolN "z" <- arbitrary
    let r  = lx .=. vx .*. ly .=. vy .*. lz .=. vz .*. emptyRecord
        ry =               ly .=. vy .*. lz .=. vz .*. emptyRecord
        rx = lx .=. vx .*.               lz .=. vz .*. emptyRecord

    return $ conjoin [
      (r .-. lx) `eq` ry,
      (r .-. ly) `eq` rx ]

  it "hBuild/hEndR" $ property $ do
    vx :: BoolN "x" <- arbitrary
    vy :: BoolN "y" <- arbitrary
    vz :: BoolN "z" <- arbitrary
    let r = hEndR (hBuild vx vy vz) `asLabelsOf` (lx .*. ly .*. lz .*. emptyProxy)
        r_ = lx .=. vx .*. ly .=. vy .*. lz .=. vz .*. emptyRecord

    return $ r `eq` r_


  it "hUncurry" $ property $ do
    vx :: BoolN "x" <- arbitrary
    vy :: BoolN "y" <- arbitrary
    vz :: BoolN "z" <- arbitrary
    return $ conjoin
      [ hUncurry (,,) (hBuild vx vy vz) `eq` (vx,vy,vz),
        hCurry (hUncurry (,,)) vx vy vz `eq` (vx,vy,vz),
        hCurry (hUncurry id) vx `eq` vx,
        hCurry ( \(a `HCons` b `HCons` HNil) -> (b,a)) vx vy `eq` (vy,vx)
      ]

  it "hCompose" $ property $ do
    vx :: BoolN "x" <- arbitrary
    vy :: BoolN "y" <- arbitrary
    vz :: BoolN "z" <- arbitrary
    return $ conjoin
      [ hCompose (,) (,) vx vy vz `eq` ((vx,vy), vz),
        hCompose id (,) vx vy `eq` (vx,vy),
        hCompose (,) id vx vy `eq` (vx,vy) ]


hTuples = do
  it "HTuple0" $ do
    HNil ^. hTuple `shouldBe` ()
    (HNil & hTuple %~ id) `shouldBe` HNil

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




-- XXX projected
v =  mkVariant (Label :: Label "x") () (Proxy :: Proxy '[Tagged "x" (), Tagged "y" Double])
vy = mkVariant (Label :: Label "y") 2.4 (Proxy :: Proxy '[Tagged "x" (), Tagged "y" Double])

vp1 :: Maybe (Variant '[Tagged "x" ()])
vp1 =  projectVariant v


vp1_ = fromJust vp1


vp2 = extendsVariant vp1_ `asTypeOf` v
vp3 = extendsVariant vp1_ `asLabelsOf` v

vp4 = (v ^? projected) `asTypeOf` vp1

vp5 = (projected # fromJust vp1) `asTypeOf` v


vm1 = v & sameLength . sameLabels . projected %~ (\x -> x :: Variant '[Tagged "x" ()])

vm2 = v & sameLength . sameLabels . projected . sameLabels %~ f2
vm3 = vy & sameLength . sameLabels . projected . sameLabels %~ f2


f2 (review hMaybied -> [pun| (x) |]) = hBuild (Just (show x)) ^?! hMaybied

f3 :: Show a => Variant '[Tagged "x" a] -> Variant '[Tagged "x" String]
f3 = unvarianted %~ show


hm1 = Proxy :: HMemberM (Tagged "y" Double) '[Tagged "x" (), Tagged "y" Char] inY => Proxy inY

v2 = fmap (`asLabelsOf` (Proxy :: Proxy '[Label "y"])) (projectVariant v)
v_id = fmap (`asLabelsOf` v) (projectVariant v)
v_id2 = fmap (`asLabelsOf` labelsOf v) (projectVariant v)

