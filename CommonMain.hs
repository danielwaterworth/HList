{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-main module that loads all modules that at least
   *compile* fine for all the models of interest. See the Makefile
   for ways to run different models.

-}

module CommonMain (

   module FakePrelude
 , module HListPrelude
 , module HListGoodies
 , module HArray
 , module HOccurs
 , module HTypeIndexed
 , module TIP
 , module TIC
 , module HZip
 , module SimpleRecords
 , module GenericTypeUnify
) where

import FakePrelude
import HListPrelude
import HListGoodies
import HArray
import HOccurs
import HTypeIndexed
import TIP
import TIC
import HZip
import SimpleRecords
import GenericTypeUnify

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
