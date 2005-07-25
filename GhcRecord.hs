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
import HZip
import Record 

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

hUnproxyLabel l (v::v) r = hUpdateAtLabel l v r
 where
  (_::Proxy v) = hLookupByLabel l r


{-----------------------------------------------------------------------------}

-- Test for values; refuse proxies

hasNoProxies :: ( HZip ls vs r
                , HasNoProxies vs
                )
             => Record r -> ()
hasNoProxies = const ()


data ProxyFound x
class HasNoProxies l
instance HasNoProxies HNil
instance Fail (ProxyFound x) => HasNoProxies (HCons (Proxy x) l)
instance HasNoProxies l => HasNoProxies (HCons e l)


{-----------------------------------------------------------------------------}

-- Narrow a record to a different record type

class  Narrow a b
 where narrow :: Record a -> Record b

instance Narrow a HNil
 where   narrow _ = emptyRecord

instance ( Narrow r r'
         , HExtract r l v
         ) => Narrow r (HCons (l,v) r')
  where
    narrow (Record r) = Record (HCons (l,v) r')
      where
        (Record r')    = narrow (Record r)
        ((l,v)::(l,v)) = hExtract r


{-----------------------------------------------------------------------------}

-- Do a type-level narrow test

constrain :: Narrow r l => Record r -> Proxy l
constrain = const proxy


{-----------------------------------------------------------------------------}

-- Narrow two records to their least-upper bound

class LubNarrow a b c | a b -> c
 where
  lubNarrow :: a -> b -> (c,c)

instance ( HZip la va a
         , HZip lb vb b
         , HTIntersect la lb lc
         , H2ProjectByLabels lc a c aout
         , H2ProjectByLabels lc b c bout
         , HRLabelSet c
         )
      => LubNarrow (Record a) (Record b) (Record c)
 where
  lubNarrow ra@(Record a) rb@(Record b) =
     ( hProjectByLabels lc ra
     , hProjectByLabels lc rb
     )
   where
    lc = hTIntersect la lb
    (la,_) = hUnzip a
    (lb,_) = hUnzip b


{-----------------------------------------------------------------------------}

-- Extension of lubNarrow to a heterogeneous list

class HLubNarrow l e | l -> e
 where
  hLubNarrow :: l -> [e]

instance ( LubNarrow h h' e
         )
      => HLubNarrow (HCons h (HCons h' HNil)) e
 where
  hLubNarrow (HCons h (HCons h' _)) = [fst ee, snd ee]
   where
    ee = lubNarrow h h'

instance ( HLubNarrow (HCons h (HCons h'' t)) e'
         , HLubNarrow (HCons h' (HCons h'' t)) e''
         , LubNarrow e' e'' e
         , HLubNarrow (HCons e (HCons h'' t)) e
         )
      => HLubNarrow (HCons h (HCons h' (HCons h'' t))) e
 where
  hLubNarrow (HCons h (HCons h' t)) = fst e : ( snd e : tail r )
   where
    e' = hLubNarrow (HCons h t)
    e'' = hLubNarrow (HCons h' t)
    e = lubNarrow (head e') (head e'')
    r = hLubNarrow (HCons (fst e) t)


{-----------------------------------------------------------------------------}

-- Helper of narrow
-- This is essentially a variation on projection.

class  HExtract r l v
 where hExtract :: r -> (l,v)

instance ( TypeEq l l1 b
         , HExtractBool b (HCons (l1,v1) r) l v
         ) => HExtract (HCons (l1,v1) r) l v
  where
   hExtract = hExtractBool (undefined::b)

class HBool b
   => HExtractBool b r l v
  where
   hExtractBool :: b -> r -> (l,v)

instance TypeCast v1 v
      => HExtractBool HTrue (HCons (l,v1) r) l v
  where
   hExtractBool _ (HCons (l,v) _) = (l,typeCast v)

instance HExtract r l v
      => HExtractBool HFalse (HCons (l1,v1) r) l v
  where 
   hExtractBool _ (HCons _ r) = hExtract r


{-----------------------------------------------------------------------------}
