{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates GHC's benefits
   with regard to HList. That is, we are able to use generic cast
   and generic type equality.

-}


-- Reusable modules
import Datatypes
import FakePrelude
import HList
import HOccurs

-- Special imports
import GenericCast
import GenericTypeEq
import GenericTypeEqBool


{-

Retrieve the Breed of an animal.

*Main> hOccurs myAnimal :: Breed
Cow

-}

{-

Normal hOccurs cannot ground result type even if it is imaginable.

*Main> hOccurs (HCons 1 HNil)

<interactive>:1:
    No instance for (HOccurs e1 (HCons e HNil))

-}

{-

hOccurs can be elaborated to ground the result type for singletons.

*Main> hOccurs' (HCons 1 HNil)
1

-}

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
