{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates hugs at work.
   This test file can also be used with ghc.

-}

import Portable
import TTypeable
import TTypeableTypeEq
import TTypeableTypeEqBool

testHOccurs = (testHOccurs1,testHOccurs2)
 where
  testHOccurs1 = hOccurs myAnimal :: Breed
  testHOccurs2 = hOccursGrounded (HCons 1 HNil)

testTypeIndexed = undefined -- (typeIdx1,typeIdx2,typeIdx3,typeIdx4,typeIdx5)
 where
  typeIdx1 = hExtend BSE myAnimal
  typeIdx2 = hUpdateByType myAnimal Sheep
  typeIdx3 = hDeleteByProxy myAnimal (HProxy::HProxy Breed)
  typeIdx4 = hProjectByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)
--  typeIdx5 = fst $ hSplitByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)

main = print $ ( testHArray
               , testHOccurs
--               , testTypeIndexed
               )
