{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates GHC's benefits
   with regard to HList. That is, we are able to use generic cast
   and generic type equality.

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

main = print $ ( testHArray
               , testHOccurs
               , testTypeIndexed
               )


{-
tuple :: ( HOccurs x l, HDelete l (HProxy x) m, HOccurs y m
         , HOccurs y l, HDelete l (HProxy y) n, HOccurs x n
         )
      => l -> (x,y)
tuple l = let
              x = hOccurs l
              m = hDelete l (hProxy x)
              y = hOccurs m
          in (x,y)
-}
