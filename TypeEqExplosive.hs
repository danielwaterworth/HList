{-# OPTIONS -fglasgow-exts #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This naive combinatorial approach to type equality is explosive.

-}


module ExplosiveTypeEq where

import FakePrelude

instance TypeEqTrue Integer Integer
instance TypeEqTrue Char Char
instance (TypeEqTrue x x', TypeEqTrue y y') => TypeEqTrue (x->y) (x'->y')
 
instance TypeEqFalse Integer Char
instance TypeEqFalse Char Integer
instance TypeEqFalse (x->y) Integer
instance TypeEqFalse (Char->Integer) (Integer->Char)
instance TypeEqFalse (Char->Char) (Char->Integer) 

-- ... exploding ...
