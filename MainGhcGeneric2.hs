{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Included for completeness' sake.
   See the corresponding appendix in the HList paper for details.

-}

module MainGhcGeneric2 where

import FakePrelude hiding (TypeEqBool,typeEqBool,proxyEqBool)
import TypeEqBoolGeneric2


{-----------------------------------------------------------------------------}

main = print ( typeEqBool True False
             , typeEqBool True "True"
             )


{-----------------------------------------------------------------------------}
