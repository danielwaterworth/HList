{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of type unification. For this
   implementation to work, we need to import it at a higher level in
   the module hierarchy than all clients of the class. Otherwise, type
   simplification will turn constraints of the form TypeUnify x y into
   the form TypeUnify x x.

   This technique works for both hugs and ghc.

-}

  
module TypeUnify where

import FakePrelude

instance TypeUnify x x
 where
  typeUnify = id
