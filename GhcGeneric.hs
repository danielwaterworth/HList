{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and generic type equality. Because of generic type equality,
   this model works with GHC but it does not work with Hugs.

-}


import Portable
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

main = print $ ( testHArray
               , testHOccurs
               , testTypeIndexed
               , testTuple
               )
