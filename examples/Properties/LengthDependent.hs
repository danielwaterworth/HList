{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-- NOTE:
--
-- To be able to compile with ghc-7.6 functions like foo are sometimes
-- called
--
-- $(varE 'foo) because this prevents ghc-7.6 from failing to typecheck
-- the expression (which fails because the number of elements in the
-- supplied HList isn't known until Properties.LengthDependentSplice)
module Properties.LengthDependent where


import Data.HList.HSort (hMSortBy)
import Data.HList.Variant (eqVariant)
import Data.HList.Record (hZipRecord2)
import Data.HList.CommonMain


import Language.Haskell.TH
import Test.QuickCheck
import Properties.Common
import Test.Hspec
import Control.Lens
import Data.List (sort,permutations)
import Data.Monoid

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


vN :: Int -> ExpQ
vN n = [| \proxy -> do
       let toV :: Gen (Record a) -> Variant a
           toV = undefined
       v <- arbitrary
       return (v `asTypeOf` toV ($(rN n) proxy))
  |]

-- specs for 1 HList of length >= 1
hl1 n1 = [| do
  let -- | generate a HList of length nMax containing elements
      -- selected from there
      genHL proxy = $(hlN n1) proxy

  it "hConcat/hAppend" $
      property $ do
        x <- genHL True
        y <- genHL True
        return $ conjoin [$(varE 'hConcat) ($(varE 'hBuild) x y) == hAppend x y,
                          $(varE 'hConcat) (hBuild x) == x]

  it "partition" $
      property $ do
        x <- genHL True
        return $ conjoin
          [hPartitionEq (Proxy :: Proxy ConstTrue) (Proxy :: Proxy ()) x `eq` (x, HNil),
           hPartitionEq (Proxy :: Proxy ConstFalse) (Proxy :: Proxy ()) x `eq` (HNil, x)]


  it "listAsHList/hList2List" $ do
      property $ do
        x <- genHL True
        return $ conjoin [
            review listAsHList x `eq` hList2List x,
            review listAsHList' x `eq` hList2List x]

  it "read/show" $
      property $ do
        xs <- genHL True
        return $ read (show xs) == xs

  it "hLength/hReplicate" $
      property $ do
        xs <- genHL True
        return $ hNat2Integral (hLength xs) == hNat2Integral $(toN n1)

  it "hInits last id" $
      property $ do
        xs <- genHL True
        return $ $(varE 'hLast) (hInits xs) == xs

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
        return $ $(varE 'hLast) (hTails xs) == HNil

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
            case $(varE 'hSplitAt) n hl of
              (hl', hNil) -> (hNil `eq` HNil) .&&. (hl' `eq` hl),

            $(varE 'hMap) (HSplitAtAppend hl) ($(varE 'hIterate) (hSucc n) HSuccF hZero) `eq` $(varE 'hReplicate) (hSucc n) hl ,
            map (\n -> uncurry (++) $ splitAt n l) [0 .. length l]      === replicate (length l+1) l
              -- the equivalent list-version
           ]

  it "hAppend empty is identity" $
      property $ do
        x <- genHL (BoolN True :: BoolN "x")
        return $ all (== x) [$(varE 'hAppend) HNil x, $(varE 'hAppend) x HNil]

  it "hReverse involution" $ do
      property $ do
        x <- genHL True
        return $ x == $(varE 'hReverse) (hReverse x)

  it "hReverse does nothing for ()" $
      let xs = hReplicate $(toN n1) ()
      in xs `shouldBe` $(varE 'hReverse) xs

  it "hInit == tail on reverse" $
      property $ do
        let hInitReference xs = hReverse (hTail (hReverse xs))
        hl <- genHL True
        return $ $(varE 'hInit) hl `eq` $(varE 'hInitReference) hl

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

  it "hZipRecord" $ property $ do
      x <- $(rN n1) (BoolN True :: BoolN "x")
      y <- $(rN n1) (BoolN True :: BoolN "y")
      let r1 = hZip x y ^. unlabeled & hList2List
          r2 = hZipRecord2 x y ^. unlabeled & hList2List
          r_ = hList2List (x ^. unlabeled) `zip` hList2List (y ^. unlabeled)

      return $ conjoin [
          r1 `eq` r_,
          r2 `eq` r_,
          hUnzip (hZip x y) `eq` (x,y) ]

  it "hZip/hUnZip" $ property $ do
      x <- genHL (BoolN True :: BoolN "x")
      y <- genHL (BoolN True :: BoolN "y")
      return $ hUnzip (hZip x y) == (x,y)

  it "hUnzip/hZip" $ property $ do
      xy <- genHL (BoolN True :: BoolN "x", BoolN True :: BoolN "y")
      let (x,y) = hUnzip xy
      return $ xy `eq` hZip x y

#if __GLASGOW_HASKELL__ < 710
 -- XXX doesn't work with ghc-7.10 RC1
  it "hZip/hZip2" $ property $ do
      x <- genHL (BoolN True :: BoolN "x")
      y <- genHL (BoolN True :: BoolN "y")
      return $ hZip x y `eq` hZip2 x y
#endif

  it "HList monoid assoc" $
    property $ do
      x <- genHL (BoolN True :: BoolN "x")
      y <- genHL (BoolN True :: BoolN "x")
      z <- genHL (BoolN True :: BoolN "x")
      return $ ((x `mappend` y) `mappend` z) `eq` (x `mappend` (y `mappend` z))

  it "HList monoid unit" $
    property $ do
      x <- genHL (BoolN True :: BoolN "x")
      return $ conjoin
        [ x === (x `mappend` mempty),
          x === (mempty `mappend` x) ]

  it "Variant monoid assoc" $ property $ do
    x <- $(vN n1) (BoolN True :: BoolN "x")
    y <- $(vN n1) (BoolN True :: BoolN "x")
    z <- $(vN n1) (BoolN True :: BoolN "x")
    return $ ((x `mappend` y) `mappend` z) `eq` (x `mappend` (y `mappend` z))

  it "Variant == /eqVariant" $ property $ do
    x <- $(vN n1) (BoolN True :: BoolN "x")
    y <- $(vN n1) (BoolN True :: BoolN "x")
    return $ conjoin [ eqVariant x y == (x == y),
                       (x == y) == (y == x) ]

  it "Variant ord" $ property $ do
    x <- $(vN n1) (BoolN True :: BoolN "x")
    y <- $(vN n1) (BoolN True :: BoolN "x")
    z <- $(vN n1) (BoolN True :: BoolN "x")
    let xyz = [x,y,z]
        s:ss = map sort (permutations xyz)
    return $ all (s ==) ss

#if __GLASGOW_HASKELL__ > 707
  -- ghc-7.6 has no ordering for Nat (only for HNat)
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
#endif

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


  it "hOccurs" $ property $ do
    w <- arbitrary :: Gen (BoolN "w")
    x <- genHL (BoolN True :: BoolN "x")
    y <- genHL (BoolN True :: BoolN "y")
    z <- genHL (BoolN True :: BoolN "z")
    let xyz = hConcat (hBuild x y z)
        hxyz = hEnd (hBuild (hHead x) (hHead y) (hHead z))
        hM v = hOccursMany xyz === hList2List v
    return $ conjoin
      [ hM x, hM y, hM z,
        hOccurs (hConcat (hBuild x (HCons w HNil) z)) === w,
        hOccursOpt xyz === (Nothing `asTypeOf` Just w)
        -- hProject hxyz === hBuild (hHead x) (hHead y)
       ]

 |]

hl2 n1 n2 = [| do
  it "splitVariant" $ property $ do
    x <- $(vN (n1 + n2)) True
    let testV :: forall n x yin yout.
               (Eq (Variant x),
                SplitVariant x yin yout,
                HSplitAt n x yin yout,
                ExtendsVariant yin x,
                ExtendsVariant yout x) =>
                Proxy n -> Variant x -> Bool
        testV n v = case $(varE 'splitVariant) v of
                      Left a -> extendsVariant (a :: Variant yin) == v
                      Right a -> extendsVariant (a :: Variant yout) == v
    return $ $(varE 'testV) $(toN n1) x


  it "hAppend equals ++" $
    property $ do
      x <- $(hlN n1) True
      y <- $(hlN n2) True
      return $ hList2List (hAppend x y) === hList2List x ++ hList2List y

  it "hTranspose involution" $ property $ do
    x <- return (error "hTranspose involution") `asTypeOf` $(hlN n1) True
    xx <- $(hlN n2) x
    return $ $(varE 'hTranspose) ($(varE 'hTranspose) xx) === xx

  it "leftUnion / unionSR" $
    property $ do
      x <- $(rN n1) True
      y <- $(rN n2) True
      let asL r = r ^. unlabeled . to hList2List
          asLs (r1,r2) = (asL r1, asL r2)
          merge xs ys = xs ++ drop (length xs) ys
          mergeSym xs ys = (merge xs ys, merge ys xs)
          eqSorted (a,b) (c,d) = sort a === sort c .&&. sort b === sort d
      return $ conjoin [
        asL (x .<++. y) === asL x `merge` asL y,
        ($(varE '(.<++.)) x x) === x,
        ($(varE '(.<++.)) y y) === y,
        asLs (unionSR x y) `eqSorted` mergeSym (asL x) (asL y),
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
#if __GLASGOW_HASKELL__ < 707
        [ $([| (x `hAppend` y) `hAppend` z |]) === $([| x `hAppend` (y `hAppend` z) |]),
          $([| (x `hAppendList` y) `hAppendList` z|]) === $([| x `hAppendList` (y `hAppendList` z)|])
        ]
#else
        [ ((x `hAppend` y) `hAppend` z) === (x `hAppend` (y `hAppend` z)),
          ((x `hAppendList` y) `hAppendList` z) === (x `hAppendList` (y `hAppendList` z))
        ]
#endif
  |]


