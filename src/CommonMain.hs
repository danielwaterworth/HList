{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-
   
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-main module that loads all modules that at least
   *compile* fine for all the models of interest. See the Makefile
   for ways to run different models.

-}

module CommonMain (

   module FakePrelude
 , module HListPrelude
 , module HArray
 , module HOccurs
 , module HTypeIndexed
 , module TIP
 , module TIC
 , module HZip
 , module Record
 , module Variant
) where

import FakePrelude
import HListPrelude
import HArray
import HOccurs
import HTypeIndexed
import TIP
import TIC
import HZip
import Record
import Variant
