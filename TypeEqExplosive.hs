{-# OPTIONS -fglasgow-exts #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This naive combinatorial approach to type equality is explosive.

-}


module ExplosiveTypeEq where

import FakePrelude

instance TypeEq Integer Integer
instance TypeEq Char Char
instance (TypeEq x x', TypeEq y y') => TypeEq (x->y) (x'->y')
 
instance TypeNotEq Integer Char
instance TypeNotEq Char Integer
instance TypeNotEq (x->y) Integer
instance TypeNotEq (Char->Integer) (Integer->Char)
instance TypeNotEq (Char->Char) (Char->Integer) 

-- ... exploding ...
