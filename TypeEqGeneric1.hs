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

   The specific encoding given here makes use of TypeCast,
   and by transitive closure therefore relies on separate compilation
   of TypeCast clients and the TypeCast instance.

   There is another encoding in TypeEqGeneric2.hs.

-}

  
module TypeEqGeneric1 where

import FakePrelude

instance TypeEq x x HTrue
instance (HBool b, TypeCast HFalse b) => TypeEq x y b
-- instance TypeEq x y HFalse -- would violate functional dependency
