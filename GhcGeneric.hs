{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and generic type equality. Because of generic type equality,
   this model works with GHC but it does not work with Hugs.

-}

module GhcGeneric (

 module CommonMain,
 module GhcSyntax,
 module GenericTypeEq,
 module GenericTypeEqBool

) where

import CommonMain
import GhcSyntax
import GenericTypeEq
import GenericTypeEqBool

testHOccurs = (testHOccurs1,testHOccurs2)
 where
  testHOccurs1 = hOccurs myAnimal :: Breed
  testHOccurs2 = hOccursGrounded (HCons 1 HNil)

testTypeIndexed = (typeIdx1,typeIdx2,typeIdx3,typeIdx4,typeIdx5)
 where
  typeIdx1 = hExtend BSE myAnimal
  typeIdx2 = hUpdateByType  typeIdx1 Sheep
  typeIdx3 = hDeleteByProxy typeIdx2 (HProxy::HProxy Breed)
  typeIdx4 = hProjectByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)
  typeIdx5 = fst $ hSplitByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)

testTuple = [testTuple1,testTuple2,testTuple3]
 where
  testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  testTuple2 = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)
  testTuple3 = let b = not $ fst $ tuple oneTrue in (1::Int,b)

testTIP = [show testTIP1, show testTIP2, show testTIP3, show testTIP4]
 where
  myTipyCow = TIP myAnimal
  animalKey :: (HOccurs Key l, HSubType l (TIP Animal)) => l -> Key
  animalKey = hOccurs
  testTIP1 = hOccurs myTipyCow :: Breed
  testTIP2 = hExtend BSE myTipyCow
  testTIP3 = hExtend Sheep $ hDeleteByProxy myTipyCow (HProxy::HProxy Breed)
  testTIP4 = hUpdateByType myTipyCow Sheep

testSimpleRecords = [ show test1 
                    , show test2
                    , show test3 
                    , show test4
                    , show test5
                    , show test6
                    ]
 where
  key   = HZero
  name  = HSucc key
  breed = HSucc name
  price = HSucc breed
  test1 = mkSimpleRecord $ HCons (key,42::Integer)
                         $ HCons (name,"Angus")
                         $ HCons (breed,Cow)
                         $ HNil 
  test2 = hLookup test1 breed
  test3 = hDelete test1 breed
  test4 = hUpdate test1 breed Sheep
  test5 = hExtend (price,8.8) test1
  test6 = hProject test5 (HCons breed (HCons price HNil))

main = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTuple
               , ( testTIP
               , ( testSimpleRecords
               ))))))
