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
    b  = proxyEq p (toProxy e')
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

class HNat n => HType2HNat e l n | e l -> n
instance (TypeEq e' e b, HType2HNatCase b e l n)
      =>  HType2HNat e (HCons e' l) n

-- Helper class

class (HBool b, HNat n) => HType2HNatCase b e l n | b e l -> n
instance HOccursNot e l => HType2HNatCase HTrue e l HZero
instance HType2HNat e l n => HType2HNatCase HFalse e l (HSucc n)

hType2HNat :: HType2HNat e l n => Proxy e -> l -> n
hType2HNat _ _ = undefined



-- Map types to naturals

class HTypes2HNats ps l ns | ps l -> ns
 where
  hTypes2HNats :: ps -> l -> ns

instance HTypes2HNats HNil l HNil
 where
  hTypes2HNats _ _ = HNil

instance ( HType2HNat   e l n
         , HTypes2HNats ps l ns
         )
      =>   HTypes2HNats (HCons (Proxy e) ps) l (HCons n ns)
 where
  hTypes2HNats (HCons p ps) l = HCons (hType2HNat p l) (hTypes2HNats ps l)


{-----------------------------------------------------------------------------}

-- Define type-indexed delete in terms of the natural-based primitive

hDeleteByProxy p l
 =
   hDeleteByHNat (hType2HNat p l) l


{-----------------------------------------------------------------------------}

-- Define type-indexed update in terms of the natural-based update

hUpdateByType e l
 =
   hUpdateByHNat (hType2HNat (toProxy e) l) e l


{-----------------------------------------------------------------------------}

-- Projection based on proxies

hProjectByProxies ps l
 =
   hProjectByHNats (hTypes2HNats ps l) l


{-----------------------------------------------------------------------------}

-- Splitting based on proxies

hSplitByProxies ps l
 =
   hSplitByHNats (hTypes2HNats ps l) l


{-----------------------------------------------------------------------------}

-- This example from the TIR paper challenges forgotten types.
-- Thanks to the HW 2004 reviewer who pointed out the value of this example.

tuple :: ( HLookup e1 l
         , HType2HNat e1 l n
         , HDeleteByHNat n l l'
         , HLookup e2 l'
         , HLookup e2 l
         , HType2HNat e2 l n'
         , HDeleteByHNat n' l l''
         , HLookup e1 l''
         ) =>
              l -> (e1, e2)

tuple l = let
              x  = hLookup l
              l' = hDeleteByProxy (toProxy x) l
              y  = hLookup l'
          in (x,y)


-- A specific tuple
oneTrue :: HCons Int (HCons Bool HNil)
oneTrue =  HCons 1   (HCons True HNil)


-- A variation that propagates all involved types

tuple' :: ( HType2HNat x l n 
          , HDeleteByHNat n l l'
          , HLookup x l
          , HLookup y l'
          )
            => l -> (n,l',x,y)

tuple' l = let
   n  = hType2HNat (toProxy x) l
   l' = hDeleteByHNat n l
   x  = hLookup l
   y  = hLookup l'
  in (n,l',x,y)


{-----------------------------------------------------------------------------}
