{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of type-level type-safe cast. For this
   implementation to work, we need to import it at a higher level in
   the module hierarchy than all clients of Cast. Otherwise, type
   simplification will turn constraints of the form Cast x y into
   the form Cast x x. This is the case for both hugs and ghc.

-}

  
module GenericTypeUnify where

import FakePrelude

instance TypeUnify x x
 where
  typeUnify = id
