{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic implementations of type equality and disequality

-}

module TypeEqGeneric where

import FakePrelude

instance            TypeEq    x x
instance Fail () => TypeNotEq x x
instance            TypeNotEq x y
