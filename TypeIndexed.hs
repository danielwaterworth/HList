{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed operations on typeful heterogeneous lists.

-}


module TypeIndexed where

import FakePrelude
import HList
import HArray
import HOccurs


{-----------------------------------------------------------------------------}

-- Map a type to a natural

class HNat n => HType2HNat l e n | l e -> n
 where
  hType2HNat :: l -> HProxy e -> n

instance ( TypeEqBool e' e b
         , HType2HNat' b l e n
         )
           => HType2HNat (HCons e' l) e n
 where
  hType2HNat (HCons e' l) p = n
   where
    b = proxyEqBool (hProxy e') p
    n = hType2HNat' b l p 


-- Helper class

class (HBool b, HNat n) => HType2HNat' b l e n | b l e -> n
 where
  hType2HNat' :: b -> l -> HProxy e -> n

instance HFreeType e l
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
      =>   HTypes2HNats l (HCons (HProxy e) ps) (HCons n ns)
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
   hUpdateByHNat l (hType2HNat l (hProxy e)) e 


{-----------------------------------------------------------------------------}

-- Projection based on proxies

hProjectByProxies l ps
 =
   hProjectByHNats l $ hTypes2HNats l ps


{-----------------------------------------------------------------------------}

-- Splitting based on proxies

hSplitByProxies l ps
 =
   hSplitByHNats l $ hTypes2HNats l ps


{-----------------------------------------------------------------------------}
