{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records -- operations that require GHC

   See Record.hs for the base module.

-}

 
module GhcRecord where

import FakePrelude
import HListPrelude
import HArray
import HZip
import Record


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

-- Casting a record up to a supertype

class UpCast a b where
    upCast :: Record a -> Record b

instance UpCast a HNil where
    upCast _ = emptyRecord

instance ( UpCast r r'
         , HExtract r l v
         )
           => UpCast r (HCons (l,v) r')
  where
    upCast (Record r) = Record (HCons (l,v) r')
      where
        (Record r')    = upCast (Record r)
        ((l,v)::(l,v)) = hExtract r

class HExtract r l v
  where 
    hExtract :: r -> (l,v)

instance ( TypeEq l l1 b
         , HExtractBool b (HCons (l1,v1) r) l v
         )
           => HExtract (HCons (l1,v1) r) l v
  where
    hExtract
      = hExtractBool (undefined::b)

class HBool b => HExtractBool b r l v
  where 
    hExtractBool :: b -> r -> (l,v)

instance TypeCast v1 v
      => HExtractBool HTrue (HCons (l,v1) r) l v
  where
    hExtractBool _ (HCons (l,v) _)
      = (l,typeCast v)

instance HExtract r l v
      => HExtractBool HFalse (HCons (l1,v1) r) l v
  where
    hExtractBool _ (HCons _ r)
      = hExtract r


{-----------------------------------------------------------------------------}
