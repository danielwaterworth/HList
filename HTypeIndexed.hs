{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed operations on typeful heterogeneous lists.

-}


module HTypeIndexed where

import FakePrelude
import HListPrelude
import HArray
import HOccurs


{-----------------------------------------------------------------------------}

class HDeleteMany e l l' | e l -> l'
 where
  hDeleteMany :: Proxy e -> l -> l'

instance HDeleteMany e HNil HNil
 where
  hDeleteMany _ HNil = HNil

instance ( HList l
         , TypeEq e e' b
         , HDeleteManyCase b e e' l l'
         )
      =>   HDeleteMany e (HCons e' l) l'
 where
  hDeleteMany p (HCons e' l) = l'
   where
    b  = proxyEq p (proxy e')
    l' = hDeleteManyCase b p e' l

class HDeleteManyCase b e e' l l' | b e e' l -> l'
 where
  hDeleteManyCase :: b -> Proxy e -> e' -> l -> l'

instance HDeleteMany e l l'
      => HDeleteManyCase HTrue e e l l'
 where
  hDeleteManyCase _ p _ l = hDeleteMany p l


instance HDeleteMany e l l'
      => HDeleteManyCase HFalse e e' l (HCons e' l')
 where
  hDeleteManyCase _ p e' l = HCons e' (hDeleteMany p l)


{-----------------------------------------------------------------------------}

-- Map a type to a natural

class HNat n => HType2HNat l e n | l e -> n
 where
  hType2HNat :: l -> Proxy e -> n

instance ( TypeEq e' e b
         , HType2HNat' b l e n
         )
           => HType2HNat (HCons e' l) e n
 where
  hType2HNat (HCons e' l) p = n
   where
    b = proxyEq (proxy e') p
    n = hType2HNat' b l p 


-- Helper class

class (HBool b, HNat n) => HType2HNat' b l e n | b l e -> n
 where
  hType2HNat' :: b -> l -> Proxy e -> n

instance HOccursNot e l
      => HType2HNat' HTrue l e HZero
 where
  hType2HNat' _ _ _ = HZero

instance HType2HNat l e n
      => HType2HNat' HFalse l e (HSucc n)
 where
  hType2HNat' _ l p = HSucc (hType2HNat l p)


-- Map types to naturals

class HTypes2HNats l ps ns | l ps -> ns
 where
  hTypes2HNats :: l -> ps -> ns

instance HTypes2HNats l HNil HNil
 where
  hTypes2HNats _ _ = HNil

instance ( HType2HNat   l e n
         , HTypes2HNats l ps ns
         )
      =>   HTypes2HNats l (HCons (Proxy e) ps) (HCons n ns)
 where
  hTypes2HNats l (HCons p ps) = HCons (hType2HNat l p) (hTypes2HNats l ps)


{-----------------------------------------------------------------------------}

-- Define type-indexed delete in terms of the natural-based primitive

hDeleteByProxy l p
 =
   hDeleteByHNat l (hType2HNat l p)


{-----------------------------------------------------------------------------}

-- Define type-indexed update in terms of the natural-based update

hUpdateByType l e
 =
   hUpdateByHNat l (hType2HNat l (proxy e)) e 


{-----------------------------------------------------------------------------}

-- Projection based on proxies

hProjectByProxies l ps
 =
   hProjectByHNats l $ hTypes2HNats l ps


{-----------------------------------------------------------------------------}

-- Splitting based on proxies

hSplitByProxies l ps
 =
   hSplitByHNats l (hTypes2HNats l ps)


{-----------------------------------------------------------------------------}

-- This example from the TIR paper challenges forgotten types.
-- Thanks to the HW 2004 reviewer who pointed out the value of this example.

tuple :: ( HLookup e1 l
         , HType2HNat l e1 n
         , HDeleteByHNat l n l'
         , HLookup e2 l'
         , HLookup e2 l
         , HType2HNat l e2 n'
         , HDeleteByHNat l n' l''
         , HLookup e1 l''
         ) =>
              l -> (e1, e2)

tuple l = let
              x  = hLookup l
              l' = hDeleteByProxy l (proxy x)
              y  = hLookup l'
          in (x,y)


-- A specific tuple
oneTrue :: HCons Int (HCons Bool HNil)
oneTrue =  HCons 1   (HCons True HNil)


-- A variation that propagates all involved types

tuple' :: ( HType2HNat l x n 
          , HDeleteByHNat l n l'
          , HLookup x l
          , HLookup y l'
          )
            => l -> (n,l',x,y)

tuple' l = let
   n  = hType2HNat l (proxy x)
   l' = hDeleteByHNat l n
   x  = hLookup l
   y  = hLookup l'
  in (n,l',x,y)


{-----------------------------------------------------------------------------}
