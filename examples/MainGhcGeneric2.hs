{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Included for completeness' sake.
   The TypeEqBoolGeneric2.hs implementation is demonstrated.

-}

module MainGhcGeneric2 where

import FakePrelude hiding (TypeEq,typeEq,proxyEq)
import TypeEqGeneric2


{-----------------------------------------------------------------------------}

main = print ( typeEq True False
             , typeEq True "True"
             )


{-----------------------------------------------------------------------------}
