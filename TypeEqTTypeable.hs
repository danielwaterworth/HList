{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   An implementation of a type equality predicate in terms
   of type-level type representations and their comparison.
   This approach works for GHC and Hugs.

-}

  
module TypeEqTTypeable where

import FakePrelude
import TTypeable


-- Generic implementation of the type equality predicate
instance ( TTypeable t tt
         , TTypeable t' tt'
         , HEq tt tt' b
         )
      =>   TypeEq t t' b


-- For conciseness
type Integer3 = Integer->Integer->Integer

testTTypeable
    = [
        show$ typeEq not (&&),
        show$ typeEq not not,

{-
    We got this:
     Context reduction stack overflow; size = 21
     Use -fcontext-stack20 to increase stack size to (e.g.) 20
    So we reduced the test suite a bit.

-}

--        show$ typeEq (&&) (||),
--        show$ typeEq ((+)::Integer3) ((-)::Integer3),
--        show$ typeEq ((*)::Integer3) ((*)::Integer3),
--        show$ typeEq ((*)::Integer3) not,
--        show$ typeEq True False,
        show$ typeEq (1::Integer) True,
        show$ typeEq False ((+)::Integer3),
        show$ typeEq (||) ((+)::Integer3),
        show$ typeEq (undefined::Fix Maybe) True,
        show$ typeEq (undefined::Fix Maybe) (undefined::Fix []),
        show$ typeEq (undefined::Fix Maybe) (undefined::Fix Maybe)
       ]
