{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and TTypeable-based type equality. This module is prepared for
   use with GHC. This model works, in principle, also for Hugs
   (see module HugsTTypeable), but some details and some test
   cases require slight variations.

-}


import Datatypes1
import CommonMain
import GhcSyntax
import TypeEqBoolTTypeable
import TypeEqTTypeable
import TypeCastGeneric1
import Label1


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

testTuple =   ( testTuple1
            , ( testTuple2
            , ( testTuple3
            , ( testTuple4
            , ( testTuple5
              )))))
 where
  testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  testTuple2 = let b = not $ fst $ tuple oneTrue in (1::Int,b)
  testTuple3 = tuple oneTrue == (1::Int,True)
  testTuple4 = ((+) (1::Int)) $ fst $ tuple oneTrue
  -- requires explicit type for tuple
  testTuple5 = ((+) (1::Int)) $ snd $ tuple oneTrue

testTIP = (testTIP1,testTIP2,testTIP3,testTIP4)
 where
  myTipyCow = TIP angus
  animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
  animalKey = hOccurs
  testTIP1 = hOccurs myTipyCow :: Breed
  testTIP2 = hExtend BSE myTipyCow
  testTIP3 = hExtend Sheep $ tipyDelete (proxy::Proxy Breed) myTipyCow
  testTIP4 = tipyUpdate Sheep myTipyCow

testRecords =   ( test1 
              , ( test2
              , ( test3 
              , ( test4
              , ( test5
              , ( test6
                ))))))
 where
  key   = firstLabel
  name  = nextLabel key
  breed = nextLabel name
  price = nextLabel breed
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
               , ( testTuple
               , ( testTIP
               , ( testRecords
               ))))))


{-----------------------------------------------------------------------------}

