{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records -- operations that (may) require GHC

   See Record.hs for the base module.

-}

 
module GhcRecord where

import FakePrelude
import HListPrelude
import HArray
import Record 
import Data.Typeable

{-----------------------------------------------------------------------------}

{-

-- The following is no longer needed: see HasField class in Record.hs

-- A look-up operation with A shielding class
-- Hugs cannot deal with such shield.
-- We get buggy "Outstanding context ..." for record access.

class Hash l r v | l r -> v
 where
  hLookupByLabel :: l -> r -> v

instance (HLookupByHNat n y v, HFind l x n, HZip x y r)
      => Hash l (Record r) v
 where
  hLookupByLabel l r = Record.hLookupByLabel l r

-}


{-----------------------------------------------------------------------------}

-- A variation on update.
-- Replace a proxy by a value of the proxied type.

hUnproxyLabel l v r = hUpdateAtLabel l v r
 where
  tpe :: a -> Proxy a -> ()
  tpe _ _ = ()
  _ = tpe v (hLookupByLabel l r)


{-----------------------------------------------------------------------------}

-- Test for values; refuse proxies

hasNoProxies :: HasNoProxies r
             => Record r -> ()
hasNoProxies = const ()


data ProxyFound x
class HasNoProxies l
instance HasNoProxies HNil
instance Fail (ProxyFound x) => HasNoProxies (HCons (Proxy x) l)
instance Fail (ProxyFound x) => HasNoProxies (HCons (LVPair lab (Proxy x)) l)
instance HasNoProxies l => HasNoProxies (HCons e l)


{-----------------------------------------------------------------------------}

-- Narrow a record to a different record type

class  Narrow a b
 where narrow :: Record a -> Record b

instance Narrow a HNil
 where   narrow _ = emptyRecord

instance ( Narrow rout r'
	 , H2ProjectByLabels (HCons l HNil) r (HCons (LVPair l v) HNil) rout
         ) => Narrow r (HCons (LVPair l v) r')
  where
    narrow (Record r) = Record (HCons f r')
      where
        (HCons f HNil,rout) = h2projectByLabels (undefined::(HCons l HNil)) r
        (Record r')    = narrow (Record rout)


{-----------------------------------------------------------------------------}

-- Narrow two records to their least-upper bound

class LubNarrow a b c | a b -> c
 where
  lubNarrow :: a -> b -> (c,c)

instance ( RecordLabels a la
	 , RecordLabels b lb
         , HTIntersect la lb lc
         , H2ProjectByLabels lc a c aout
         , H2ProjectByLabels lc b c bout
         , HRLabelSet c
         )
      => LubNarrow (Record a) (Record b) (Record c)
 where
  lubNarrow ra@(Record a) rb@(Record b) =
     ( hProjectByLabels (undefined::lc) ra
     , hProjectByLabels (undefined::lc) rb
     )


{-----------------------------------------------------------------------------}

-- List constructors that also LUB together

data NilLub
nilLub = undefined :: NilLub

class ConsLub h t l | h t -> l
 where
  consLub :: h -> t -> l

instance ConsLub e  NilLub [e]
 where
  consLub h _ = [h]

instance LubNarrow e0 e1 e2 => ConsLub e0 [e1] [e2]
 where
  consLub h t = fst (head z) : map snd (tail z)
   where
    z = map (lubNarrow h) (undefined:t)


{-----------------------------------------------------------------------------}

-- Extension of lubNarrow to a heterogeneous list

class HLub l e | l -> e
 where
  hLub :: l -> [e]

instance ( LubNarrow h h' e
         )
      => HLub (HCons h (HCons h' HNil)) e
 where
  hLub (HCons h (HCons h' _)) = [fst ee, snd ee]
   where
    ee = lubNarrow h h'

instance ( HLub (HCons h (HCons h'' t)) e'
         , HLub (HCons h' (HCons h'' t)) e''
         , LubNarrow e' e'' e
         , HLub (HCons e (HCons h'' t)) e
         )
      => HLub (HCons h (HCons h' (HCons h'' t))) e
 where
  hLub (HCons h (HCons h' t)) = fst e : ( snd e : tail r )
   where
    e' = hLub (HCons h t)
    e'' = hLub (HCons h' t)
    e = lubNarrow (head e') (head e'')
    r = hLub (HCons (fst e) t)


{-----------------------------------------------------------------------------}

-- Typeable instances

hNilTcName = mkTyCon "HList.HNil"
instance Typeable HNil
 where
  typeOf _ = mkTyConApp hNilTcName []

hConsTcName = mkTyCon "HList.HCons"
instance (Typeable x, Typeable y) => Typeable (HCons x y)
 where
  typeOf (HCons x y)
   = mkTyConApp hConsTcName [ typeOf x, typeOf y ]

recordTcName = mkTyCon "HList.Record"
instance Typeable x => Typeable (Record x)
 where
  typeOf (Record x)
   = mkTyConApp recordTcName [ typeOf x ]

hFieldTcName = mkTyCon "HList.F"
instance (Typeable x, Typeable y) => Typeable (LVPair x y)
 where
  typeOf _
   = mkTyConApp hFieldTcName [ typeOf (undefined::x), typeOf (undefined::y)  ]

proxyTcName = mkTyCon "HList.Proxy"
instance Typeable x => Typeable (Proxy x)
 where
  typeOf (_::Proxy x)
   = mkTyConApp proxyTcName [ typeOf (undefined::x) ]


{-----------------------------------------------------------------------------}
