{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   An implementation of a type equality predicate in terms
   of type-level type representations and their comparison.
   This approach works for GHC and Hugs.

-}

  
module TypeEqBoolTTypeable where

import FakePrelude
import TTypeable


-- Generic implementation of the type equality predicate
instance ( TTypeable t tt
         , TTypeable t' tt'
         , TTypeableEqBool tt tt' b
         )
      =>   TypeEqBool t t' b


-- For conciseness
type Integer3 = Integer->Integer->Integer

testTTypeable
    = [
        show$ typeEqBool not (&&),
        show$ typeEqBool not not,

{-
    We got this:
     Context reduction stack overflow; size = 21
     Use -fcontext-stack20 to increase stack size to (e.g.) 20
    So we reduced the test suite a bit.

-}

--        show$ typeEqBool (&&) (||),
--        show$ typeEqBool ((+)::Integer3) ((-)::Integer3),
--        show$ typeEqBool ((*)::Integer3) ((*)::Integer3),
--        show$ typeEqBool ((*)::Integer3) not,
--        show$ typeEqBool True False,
        show$ typeEqBool (1::Integer) True,
        show$ typeEqBool False ((+)::Integer3),
        show$ typeEqBool (||) ((+)::Integer3),
        show$ typeEqBool (undefined::Fix Maybe) True,
        show$ typeEqBool (undefined::Fix Maybe) (undefined::Fix []),
        show$ typeEqBool (undefined::Fix Maybe) (undefined::Fix Maybe)
       ]
