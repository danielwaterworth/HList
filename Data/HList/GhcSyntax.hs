{-# LANGUAGE FlexibleContexts #-}
{-
   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some dedicated infix operators at the type and the value level.
-}

module Data.HList.GhcSyntax where

import Data.HList.HArray (HUpdateAtHNat())
import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HOccurs
import Data.HList.Record
import Data.HList.GhcRecord
import Data.HList.TIP
import Data.HList.TIC


{-----------------------------------------------------------------------------}

-- Convenience notation for type sequences

infixr 2 :*:
infixr 2 .*.

type e :*: l = HCons e l

(.*.) :: HExtend e l l' => e -> l -> l'
(.*.) =  hExtend


{-----------------------------------------------------------------------------}

-- Convenience notation for records
-- Many signatures are deliberately omitted. They should be inferred.
-- There is no point of writing the same thing in terms and in types.

infixr 4 :=:
type l :=: v = LVPair l v

infixr 4 .=.
(.=.) :: l -> v -> LVPair l v
l .=. v = newLVPair l v

infixr 3 .!.
(.!.) :: (HasField l r v) => r -> l -> v
r .!. l =  hLookupByLabel l r

infixl 1 .-.
r .-. l =  hDeleteAtLabel l r

infixl 1 .@.
r .@. f@(LVPair v) =  hUpdateAtLabel (labelLVPair f) v r

infixr 1 .^.
f@(LVPair v) .^. r = hUnproxyLabel (labelLVPair f) v r

infixr 1 .<.
f@(LVPair v) .<. r = hTPupdateAtLabel (labelLVPair f) v r

infixl 1 .<++.
r .<++. r' = hLeftUnion r r'


{-----------------------------------------------------------------------------}

-- Convenience notation for TIRs

infixr 2 :+:
infixr 2 .+.

type e :+: l = HCons (Proxy e) l

e .+. r = hExtend (toProxy e) r


{-----------------------------------------------------------------------------}
