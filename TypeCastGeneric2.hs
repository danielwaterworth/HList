{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of a type-safe cast. The specific coding
   here is only shown for completeness' sake. We actually favour the
   encoding from TypeCastGeneric1.hs for its conciseness. The specific
   coding here does not rely on separate compilation (while
   TypeCastGeneric1.hs does), but on some other tricks.

-}

  
module TypeCastGeneric2 where

-- We make everything self-contained to show that separate compilation
-- is not needed.

import FakePrelude hiding (TypeCast,typeCast)


{-----------------------------------------------------------------------------}

-- The actual encoding

class TypeCast   a b   | a -> b   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x


{-----------------------------------------------------------------------------}
