{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-top-level file that loads all across-model modules.

-}

module Portable (

   module Datatypes
 , module FakePrelude
 , module HList
 , module HOccurs
 , module GenericCast
 , module GenericTypeEq
) where

import Datatypes
import FakePrelude
import HList
import HOccurs
import GenericCast
import GenericTypeEq

--
-- In the remainder of the module,
-- we comment on some standard test cases.
-- These test cases work for all models that import HListMain.hs.
--

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

*Main> hOccursGrounded (HCons 1 HNil)
1

-}
