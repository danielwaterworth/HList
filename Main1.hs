{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a top-level test file that demonstrates GHC's benefits
   with regard to HList. That is, we are able to use generic cast
   and generic type equality.

-}


import Portable
import GenericTypeEqBool



{-
tuple :: ( HOccurs x l, HDelete l (HProxy x) m, HOccurs y m
         , HOccurs y l, HDelete l (HProxy y) n, HOccurs x n
         )
      => l -> (x,y)
tuple l = let
              x = hOccurs l
              m = hDelete l (hProxy x)
              y = hOccurs m
          in (x,y)
-}


main = print $ ( hOccurs myAnimal :: Breed
               , hOccursGrounded (HCons 1 HNil)
               )
