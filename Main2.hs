{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates hugs at work.
   This test file can also be used with ghc.

-}


-- Reusable modules
import Datatypes
import FakePrelude
import HList
import HOccurs

-- Special imports
import GenericCast
import GenericTypeEq
import TTypeable

{-

Retrieve the Breed of an animal.

Main> hOccurs myAnimal :: Breed
Cow

-}

{-

Normal hOccurs cannot ground result type even if it is imaginable.

Main> hOccurs (HCons 1 HNil)
ERROR - Unresolved overloading
*** Type       : HOccurs a HNil => a
*** Expression : hOccurs (HCons 1 HNil)

-}

{-

hOccurs can be elaborated to ground the result type for singletons.

Main> hOccurs' (HCons 1 HNil)
1

-}
