{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and TTypeable-based type equality. This module is prepared for use
   with Hugs. This model works in principle also for GHC (see module
   GHCTTypeable), but some details and some test cases require slight
   variations. In particular, we show comment lines for test cases
   that work with GHC but that do not work with Hugs.

-}


import Portable
import TTypeableTypeEq
import TTypeableTypeEqBool

testHOccurs = (testHOccurs1,testHOccurs2)
 where
  testHOccurs1 = hOccurs myAnimal :: Breed
  testHOccurs2 = hOccursGrounded (HCons 1 HNil)

testTypeIndexed = (typeIdx1,typeIdx3,typeIdx4)
 where
   typeIdx1 = hExtend BSE myAnimal
-- typeIdx2 = hUpdateByType  typeIdx1 Sheep
   typeIdx3 = hDeleteByProxy myAnimal (HProxy::HProxy Breed)
   typeIdx4 = hProjectByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)
-- typeIdx5 = fst$ hSplitByProxies myAnimal (HCons (HProxy::HProxy Breed) HNil)

-- testTuple = [testTuple1,testTuple2,testTuple3]
--  where
--   testTuple1 = let (a,b) = tuple oneTrue in (a+(1::Int), not b)
--   testTuple2 = let (n,l,a,b) = tuple' oneTrue in (a+(1::Int), not b)
--   testTuple3 = let b = not $ fst $ tuple oneTrue in (1::Int,b)

main = print $ ( testHArray
               , testHOccurs
               , testTypeIndexed
               -- , testTuple
               )
