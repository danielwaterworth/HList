{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and generic type equality. Because of generic type equality,
   this model works with GHC but it does not work with Hugs.

-}

module MainGhcGeneric1 (

 module Datatypes2,
 module CommonMain,
 module GhcSyntax,
 module TypeEqGeneric,
 module TypeEqBoolGeneric1,
 module Label3

) where

import Datatypes2
import CommonMain
import GhcSyntax
import TypeEqGeneric
import TypeEqBoolGeneric1
import Label3


{-----------------------------------------------------------------------------}

type Animal =  HCons Key
              (HCons Name
              (HCons Breed
              (HCons Price
               HNil)))

myAnimal :: Animal
myAnimal =  HCons (Key 42)
           (HCons (Name "Angus")
           (HCons  Cow
           (HCons (Price 75.5)
            HNil)))

{-

HList> hFoldr (HSeq HShow) (return () :: IO ()) myAnimal
Key 42
Name "Angus"
Cow
Price 75.5

HList> hAppend myAnimal myAnimal
HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) (HCons (Key
42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))))))

-}

testHArray = (myProj1,myProj2,myProj3,myProj4)
 where
  myProj1 = hProjectByHNats myAnimal (HCons HZero (HCons HZero HNil))
  myProj2 = hProjectByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))
  myProj3 = hProjectAwayByHNats myAnimal (HCons HZero HNil)
  myProj4 = hSplitByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))

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

testHOccurs = (testHOccurs1,testHOccurs2,testHOccurs3)
 where
  testHOccurs1 = hOccurs myAnimal :: Breed
  testHOccurs2 = hLookup (HCons 1 HNil)
  testHOccurs3 = null $ hLookup (HCons [] HNil)

testTypeIndexed = (typeIdx1,typeIdx2,typeIdx3,typeIdx4,typeIdx5)
 where
  typeIdx1 = hExtend BSE myAnimal
  typeIdx2 = hUpdateByType  typeIdx1 Sheep
  typeIdx3 = hDeleteByProxy typeIdx2 (Proxy::Proxy Breed)
  typeIdx4 = hProjectByProxies myAnimal (HCons (Proxy::Proxy Breed) HNil)
  typeIdx5 = fst $ hSplitByProxies myAnimal (HCons (Proxy::Proxy Breed) HNil)

testTuple = (testTuple1,testTuple2,testTuple3)
 where
  testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  testTuple2 = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)
  testTuple3 = let b = not $ fst $ tuple oneTrue in (1::Int,b)

testTIP = (testTIP1,testTIP2,testTIP3,testTIP4)
 where
  myTipyCow = TIP myAnimal
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

type AnimalCol = Key :+: Name :+: Breed :+: Price :+: HNil

testTIC = (myCol,test2,test3)
 where
  myCol = mkTIC Cow :: TIC AnimalCol
  test2 = unTIC myCol :: Maybe Breed
  test3 = unTIC myCol :: Maybe Price

{-

myCol = mkTIC Cow :: TIC AnimalCol

*TIC> unTIC myCol :: Maybe Breed
Just Cow
*TIC> unTIC myCol :: Maybe Price
Nothing
*TIC> mkTIC "42" :: TIC AnimalCol
Type error ...
*TIC> unTIC myCol :: Maybe String
Type error ...

-}


{-----------------------------------------------------------------------------}

main = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTuple
               , ( testTIP
               , ( testRecords
               , ( testTIC
               )))))))


{-----------------------------------------------------------------------------}
