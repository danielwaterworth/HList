{-# OPTIONS -fglasgow-exts #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some dedicated infix operators at the type and the value level.

-}


module GhcSyntax where

import FakePrelude
import HListPrelude
import Record
import TIP
import TIC


{-----------------------------------------------------------------------------}

-- Convenience notation for type sequences

infixr 1 :*:
infixr 1 .*.
 
type e :*: l = HCons e l
 
(.*.) :: HExtend e l l' => e -> l -> l'
(.*.) =  hExtend


{-----------------------------------------------------------------------------}

-- Convenience notation for records
 
infixr 4 :=:
type l :=: v = (l,v)
 
infixr 4 .=.
l .=. v = (l,v)

l ! x =  hLookupByLabel l x


{-----------------------------------------------------------------------------}

-- Convenience notation for TIRs

infixr 3 :+:
infixr 3 .+.

type e :+: l = HCons (Proxy e) l

(.+.) :: HTypeProxied (HCons (Proxy e) l)
      => e -> TIP l -> TIP (HCons (Proxy e) l)
e .+. r = hExtend (toProxy e) r


{-----------------------------------------------------------------------------}
