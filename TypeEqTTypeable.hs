{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Implementations of type equality and disequality based on TTypeable.
   This approach works for GHC and Hugs.

-}

module TypeEqTTypeable where

import FakePrelude
import TTypeable
import TypeEqBoolTTypeable

instance TypeEqBool x y HTrue  => TypeEq x y
instance TypeEqBool x y HFalse => TypeNotEq x y
