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
  myProj1 = hProjectByHNats angus (HCons HZero (HCons HZero HNil))
  myProj2 = hProjectByHNats angus (HCons HZero (HCons (HSucc HZero) HNil))
  myProj3 = hProjectAwayByHNats angus (HCons HZero HNil)
  myProj4 = hSplitByHNats angus (HCons HZero (HCons (HSucc HZero) HNil))

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
  testHOccurs2 = hLookup (HCons 1 HNil)
  testHOccurs3 = null $ hLookup (HCons [] HNil)
  testHOccurs4 = hProject angus :: (HCons Key (HCons Name HNil))

testTypeIndexed =   ( typeIdx1
                  , ( typeIdx2
                  , ( typeIdx3
                  , ( typeIdx4
                  , ( typeIdx5
                  , ( typeIdx6 ))))))
 where
  typeIdx1 = hDeleteMany (Proxy::Proxy Name) angus
  typeIdx2 = hExtend BSE angus
  typeIdx3 = hUpdateByType  typeIdx1 Sheep
  typeIdx4 = hDeleteByProxy typeIdx2 (Proxy::Proxy Breed)
  typeIdx5 = hProjectByProxies angus (HCons (Proxy::Proxy Breed) HNil)
  typeIdx6 = fst $ hSplitByProxies angus (HCons (Proxy::Proxy Breed) HNil)

testTuple = (testTuple1,testTuple2,testTuple3,testTuple4)
 where
  testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  testTuple2 = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)
  testTuple3 = let b = not $ fst $ tuple oneTrue in (1::Int,b)
-- The following test case fails with Hugs
-- testTuple4 = tuple oneTrue == (1,True)
  testTuple4 = True

testTIP = [show testTIP1, show testTIP2, show testTIP3, show testTIP4]
 where
  myTipyCow = TIP angus
  animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
  animalKey = hOccurs
  testTIP1 = hOccurs myTipyCow :: Breed
  testTIP2 = hExtend BSE myTipyCow
  testTIP3 = hExtend Sheep $ tipyDelete myTipyCow (Proxy::Proxy Breed)
  testTIP4 = tipyUpdate myTipyCow Sheep

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
  test2 = hLookupByLabel test1 breed
  test3 = hDeleteByLabel test1 breed
  test4 = hUpdateByLabel test1 breed Sheep
  test5 = hExtend (price,8.8) test1
  test6 = hProjectByLabels test5 (HCons breed (HCons price HNil))


{-----------------------------------------------------------------------------}

main = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTuple
               , ( testTIP
               , ( testRecords
               ))))))


{-----------------------------------------------------------------------------}
