{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Included for completeness' sake.
   The TypeEqBoolGeneric2.hs implementation is demonstrated.

-}

module MainGhcGeneric2 where

import Data.HList


{-----------------------------------------------------------------------------}

main = print ( hEq True False
             , hEq True "True"
             )


{-----------------------------------------------------------------------------}
