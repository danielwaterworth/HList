{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of a type equality predicate. The given
   implementation only works for GHC. It relies on two properties
   of GHC instance selection: (i) selection is lazy, and the negation
   of the constraints of the more specific instance is assumed for
   the more general instance.

-}

  
module GenericTypeEqBool where

import FakePrelude

instance TypeEqBool x x HTrue
instance Cast HFalse b => TypeEqBool x y b
