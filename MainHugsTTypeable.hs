{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and TTypeable-based type equality. This module is prepared for use
   with Hugs. This model works in principle also for GHC (see module
   GHCTTypeable) perhaps modulo some slight differences in test cases.

-}


import Datatypes1
import CommonMain
import TypeEqTTypeable
import TypeEqBoolTTypeable
import TypeCastGeneric1
import Label2


{-----------------------------------------------------------------------------}

type Animal =  HCons Key
              (HCons Name
              (HCons Breed
              (HCons Price
               HNil)))

angus :: Animal
angus =  HCons (Key 42)
           (HCons (Name "Angus")
           (HCons  Cow
           (HCons (Price 75.5)
            HNil)))

{-

HList> hFoldr (HSeq HShow) (return () :: IO ()) angus
Key 42
Name "Angus"
Cow
Price 75.5

HList> hAppend angus angus
HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) (HCons (Key
42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))))))

-}

testHArray = (myProj1,myProj2,myProj3,myProj4)
 where
  myProj1 = hProjectByHNats (HCons hZero (HCons hZero HNil)) angus
  myProj2 = hProjectByHNats (HCons hZero (HCons (hSucc hZero) HNil)) angus
  myProj3 = hProjectAwayByHNats (HCons hZero HNil) angus
  myProj4 = hSplitByHNats (HCons hZero (HCons (hSucc hZero) HNil)) angus

{-

*HArray> myProj1
HCons (Key 42) (HCons (Key 42) HNil)

*HArray> myProj2
HCons (Key 42) (HCons Cow HNil)

*HArray> myProj3
HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil))

*HArray> myProj4
(HCons (Key 42) (HCons (Name "Angus") HNil),HCons Cow (HCons (Price 75.5) HNil)

-}

testHOccurs = (testHOccurs1,testHOccurs2,testHOccurs3,testHOccurs4)
 where
  testHOccurs1 = hOccurs angus :: Breed
  testHOccurs2 = hOccurs (TIP (HCons 1 HNil))
  testHOccurs3 = null $ hOccurs (TIP (HCons [] HNil))
  testHOccurs4 = hProject angus :: (HCons Key (HCons Name HNil))

testTypeIndexed =   ( typeIdx1
                  , ( typeIdx2
                  , ( typeIdx3
                  , ( typeIdx4
                  , ( typeIdx5
                  , ( typeIdx6 ))))))
 where
  typeIdx1 = hDeleteMany (proxy::Proxy Name) angus
  typeIdx2 = hExtend BSE angus
  typeIdx3 = hUpdateAtType Sheep typeIdx1
  typeIdx4 = hDeleteAtProxy (proxy::Proxy Breed) typeIdx2
  typeIdx5 = hProjectByProxies (HCons (proxy::Proxy Breed) HNil) angus
  typeIdx6 = fst $ hSplitByProxies (HCons (proxy::Proxy Breed) HNil) angus

-- Test for tuple omitted.
-- Too fragile.

myTipyCow = TIP angus
animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
animalKey = hOccurs

testTIP = [show testTIP1, show testTIP2, show testTIP3, show testTIP4]
 where
  testTIP1 = hOccurs myTipyCow :: Breed
  testTIP2 = hExtend BSE myTipyCow
  testTIP3 = hExtend Sheep $ tipyDelete (proxy::Proxy Breed) myTipyCow
  testTIP4 = tipyUpdate Sheep myTipyCow

data MyNS = MyNS -- a name space for record labels

testRecords =   ( test1 
              , ( test2
              , ( test3 
              , ( test4
              , ( test5
              , ( test6
                ))))))
 where
  key   = firstLabel MyNS  "key"
  name  = nextLabel  key   "name"
  breed = nextLabel  name  "breed"
  price = nextLabel  breed "price"
  test1 = mkRecord $ HCons (key,42::Integer)
                   $ HCons (name,"Angus")
                   $ HCons (breed,Cow)
                   $ HNil 
  test2 = hLookupByLabel breed test1
  test3 = hDeleteAtLabel breed test1
  test4 = hUpdateAtLabel breed Sheep test1
  test5 = hExtend (price,8.8) test1
  test6 = hProjectByLabels (HCons breed (HCons price HNil)) test5


{-----------------------------------------------------------------------------}

main = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTIP
               , ( testRecords
               )))))


{-----------------------------------------------------------------------------}
