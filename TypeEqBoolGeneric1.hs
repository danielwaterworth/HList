{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of a type equality predicate. The given
   implementation only works for GHC. It relies on two properties
   of GHC instance selection: (i) selection is lazy, and the negation
   of the constraints of the more specific instance is assumed for
   the more general instance.

   The specific encoding given here makes use of TypeUnify,
   and by transitive closure therefore relies on separate compilation
   of TypeUnify clients and the TypeUnify instance.

   There is another encoding in TypeEqBoolGeneric2.hs.

-}

  
module TypeEqBoolGeneric1 where

import FakePrelude

instance TypeEqBool x x HTrue
instance TypeUnify HFalse b => TypeEqBool x y b
