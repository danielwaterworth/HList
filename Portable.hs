{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-top-level file that loads all across-model modules.
   This file is not enough for a session. Use Main?.hs.

-}

module Portable (

   module Datatypes
 , module FakePrelude
 , module HList
 , module HArray
 , module HOccurs
 , module TypeIndexed
 , module GenericCast
) where

import Datatypes
import FakePrelude
import HList
import HArray
import HOccurs
import TypeIndexed
import GenericCast

--
-- In the remainder of the module,
-- we comment on some standard test cases.
-- These test cases work for all models that import HListMain.hs.
--

{-

Retrieve the Breed of an animal.

ghci-or-hugs> hOccurs myAnimal :: Breed
Cow

-}

{-

Normal hOccurs cannot ground result type even if it is imaginable.

ghci-or-hugs> hOccurs (HCons 1 HNil)

<interactive>:1:
    No instance for (HOccurs e1 (HCons e HNil))

-}

{-

hOccurs can be elaborated to ground the result type for singletons.

ghci-or-hugs> hOccursGrounded (HCons 1 HNil)
1

-}
