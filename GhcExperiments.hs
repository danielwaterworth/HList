{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This module gathers experiments that do not work with Hugs.

-}

module GhcExperiments where

import FakePrelude
import HListPrelude

class HDeleteMany e l l' | e l -> l'
 where
  hDeleteMany :: Proxy e -> l -> l'

instance HDeleteMany e HNil HNil
 where
  hDeleteMany _ HNil = HNil

instance (HList l, HDeleteMany e l l')
      => HDeleteMany e (HCons e l) l'
 where
  hDeleteMany p (HCons _ l) = hDeleteMany p l

{-

-- Hopelessly overlapping

instance (HList l, HDeleteMany e l l')
      => HDeleteMany e (HCons e' l) (HCons e' l')
 where
  hDeleteMany p (HCons e' l)
   =
     HCons e' (hDeleteMany p l)

-}

instance ( HList l
         , HDeleteMany e l l'
         , TypeCast (HCons e' l') l''
         )
      =>   HDeleteMany e (HCons e' l) l''
 where
  hDeleteMany p (HCons e' l)
   =
     typeCast (HCons e' (hDeleteMany p l))
