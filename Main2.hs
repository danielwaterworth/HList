{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates hugs at work.
   This test file can also be used with ghc.

-}

import Portable
import TTypeable

main = print $ ( hOccurs myAnimal :: Breed
               , hOccursGrounded (HCons 1 HNil)
               )
