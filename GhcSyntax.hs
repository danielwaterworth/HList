{-# OPTIONS -fglasgow-exts #-}

module GhcSyntax where

import FakePrelude
import HListPrelude
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


-- Convenience notation for TIRs

infixr 3 :+:
infixr 3 .+.

type e :+: l = HCons (Proxy e) l

(.+.) :: HTypeProxied (HCons (Proxy e) l)
      => e -> TIP l -> TIP (HCons (Proxy e) l)
e .+. r = hExtend (proxy e) r


{-----------------------------------------------------------------------------}
