{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of a type equality predicate. The given
   implementation only works for GHC. The specific coding here is only
   show for completeness' sake. We actually favour the encoding from
   TypeEqBoolGeneric1.hs for its conciseness. The specific coding here
   does not rely on separate compilation (as TypeEqBoolGeneric1.hs
   does), but on some other tricks explained in an appendix of the
   HList paper.

-}

  
module TypeEqBoolGeneric2 where

-- We make everything self-contained to show that separate compilation
-- is not needed. Also, we need a new class constraint for TypeEqBool,
-- (unless we are again employ separate compilation in some ways) so
-- that instance selection of its generic instance within client code
-- of TypeEqBool does not issue problems with the instance
-- constraints.

import FakePrelude hiding (TypeEqBool,typeEqBool,proxyEqBool)


-- Re-enabled for testing

typeEqBool :: TypeEqBool t t' b => t -> t' -> b
typeEqBool = undefined


{-----------------------------------------------------------------------------}

-- The actual encoding

class TypeEqBool' () x y b => TypeEqBool x y b | x y -> b
class TypeEqBool' q x y b | q x y -> b
class TypeEqBool'' q x y b | q x y -> b
instance TypeEqBool' () x y b => TypeEqBool x y b
instance TypeEqBool' () x x HTrue
instance TypeEqBool'' q x y b => TypeEqBool' q x y b
instance TypeEqBool'' () x y HFalse


{-----------------------------------------------------------------------------}
