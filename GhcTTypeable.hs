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


import Portable
import TTypeableTypeEq
import TTypeableTypeEqBool

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

testTuple = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
testTuple' = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)

main = print $ ( testHArray
               , testHOccurs
               , testTypeIndexed
               , testTuple 
               , testTuple'
               )
