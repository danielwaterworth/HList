{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Implementations of type equality and disequality based on TTypeable.
   This approach works for GHC and Hugs.

-}

module TypeEqBoolTTypeable where

import FakePrelude
import TTypeable
import TypeEqTTypeable

instance TypeEq x y HTrue  => TypeEqTrue x y
instance TypeEq x y HFalse => TypeEqFalse x y
