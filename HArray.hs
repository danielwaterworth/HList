{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Array-like access to HLists.

 -}

 
module HArray where

import FakePrelude
import HListPrelude
import HListGoodies


{-----------------------------------------------------------------------------}

-- A lookup operation

class HLookupByHNat l i e | l i -> e
 where
  hLookupByHNat :: l -> i -> e

instance HLookupByHNat (HCons e l) HZero e
 where
  hLookupByHNat (HCons e _) _ = e

instance HLookupByHNat l n e'
      => HLookupByHNat (HCons e l) (HSucc n) e'
 where
  hLookupByHNat (HCons _ l) (HSucc n) = hLookupByHNat l n


{-----------------------------------------------------------------------------}

-- A delete operation

class HDeleteByHNat l i l' | l i -> l'
 where
  hDeleteByHNat :: l -> i -> l'

instance HDeleteByHNat (HCons e l) HZero l
 where
  hDeleteByHNat (HCons _ l) HZero = l

instance HDeleteByHNat l n l'
      => HDeleteByHNat (HCons e l) (HSucc n) (HCons e l')
 where
  hDeleteByHNat (HCons e l) (HSucc n) = HCons e (hDeleteByHNat l n)


{-----------------------------------------------------------------------------}

-- An update operation

class HUpdateByHNat l i e l' | l i e -> l', l' i -> e
 where
  hUpdateByHNat :: l -> i -> e -> l'

instance HUpdateByHNat (HCons e l) HZero e' (HCons e' l)
 where
  hUpdateByHNat (HCons e l) _ e' = HCons e' l

instance HUpdateByHNat l n e' l'
      => HUpdateByHNat (HCons e l) (HSucc n) e' (HCons e l')
 where
  hUpdateByHNat (HCons e l) (HSucc n) e'
               = HCons e (hUpdateByHNat l n e')


{-----------------------------------------------------------------------------}

-- Splitting an array according to indices

hSplitByHNats l = hSplitByHNats' (hFlag l)

class HSplitByHNats' l il l' l'' | l il -> l' l''
 where
  hSplitByHNats' :: l -> il -> (l',l'')

instance HSplit l l' l''
      => HSplitByHNats' l HNil HNil l'
 where
  hSplitByHNats' l HNil = (HNil,l')
   where
    (l',l'') = hSplit l

instance ( HLookupByHNat l i (e,b)
         , HUpdateByHNat l i (e,HFalse) l'''
         , HSplitByHNats' l''' il l' l''
         )
      =>   HSplitByHNats' l (HCons i il) (HCons e l') l''
 where
  hSplitByHNats' l (HCons i il) = (HCons e l',l'')
   where
    (e,_)    = hLookupByHNat  l i
    l'''     = hUpdateByHNat  l i (e,HFalse)
    (l',l'') = hSplitByHNats' l''' il


{-----------------------------------------------------------------------------}

-- Another projection operation

class HProjectByHNats l il l' | l il -> l'
 where
  hProjectByHNats :: l -> il -> l'

instance HProjectByHNats HNil HNil HNil
 where
  hProjectByHNats _ _ = HNil

instance HProjectByHNats (HCons e l) HNil HNil
 where
  hProjectByHNats _ _ = HNil

instance ( HLookupByHNat (HCons e l) i e'
         , HProjectByHNats (HCons e l) il l'
         )
         => HProjectByHNats (HCons e l) (HCons i il) (HCons e' l')
 where
  hProjectByHNats l (HCons i il) = HCons e' l'
   where e' = hLookupByHNat l i
         l' = hProjectByHNats l il
 

{-----------------------------------------------------------------------------}

-- The complement of projection

class HProjectAwayByHNats l il l' | l il -> l'
 where
  hProjectAwayByHNats :: l -> il -> l'

instance ( HLength l len
         , HBetween len nats
         , HDiff nats il il'
         , HProjectByHNats l il' l'
         )
           => HProjectAwayByHNats l il l'
 where
  hProjectAwayByHNats l il = l'
   where
    len  = hLength l
    nats = hBetween len
    il'  = hDiff nats il
    l'   = hProjectByHNats l il'


{-----------------------------------------------------------------------------}

-- Generate naturals from 1 to x - 1

class HBetween x y | x -> y
 where
  hBetween :: x -> y

instance HBetween (HSucc HZero) (HCons HZero HNil)
 where
  hBetween _ = HCons HZero HNil

instance ( HBetween (HSucc x) y
         , HAppend y (HCons (HSucc x) HNil) z
         , HList y
         )
           => HBetween (HSucc (HSucc x)) z
 where
  hBetween (HSucc x) = hBetween x `hAppend` HCons x HNil


-- Set-difference on naturals

class HDiff x y z | x y -> z
 where
  hDiff :: x -> y -> z

instance HDiff HNil x HNil
 where
  hDiff _ _ = HNil

instance ( HOrdMember e y b
         , HDiff x y z
         , HCond b z (HCons e z) z'
         )
           => HDiff (HCons e x) y z'
 where
  hDiff (HCons e x) y = z'
   where z' = hCond b z (HCons e z)
         b  = hOrdMember e y
         z  = hDiff x y


-- Membership test for types with HOrd instances
-- This special type equality/comparison is entirely pure!

class HOrdMember e l b | e l -> b
 where
  hOrdMember :: e -> l -> b

instance HOrdMember e HNil HFalse
 where
  hOrdMember _ _ = HFalse

instance ( HEq e e' b1
         , HOrdMember e l b2
         , HOr b1 b2 b
         )
           => HOrdMember e (HCons e' l) b
 where
  hOrdMember e (HCons e' l) = hOr b1 b2
   where
    b1 = hEq e e'
    b2 = hOrdMember e l


{-----------------------------------------------------------------------------}

-- Length operation

class HLength l s | l -> s
 where
  hLength :: l -> s

instance HLength HNil HZero
 where
  hLength _ = HZero

instance HLength l n
      => HLength (HCons a l) (HSucc n)
 where
  hLength (HCons _ l) = HSucc (hLength l)


{-----------------------------------------------------------------------------}

-- Bounded lists

class HMaxLength l s
instance (HLength l s', HLt s' (HSucc s) HTrue) => HMaxLength l s 

class HMinLength l s
instance (HLength l s', HLt s (HSucc s') HTrue) => HMinLength l s 

class HSingleton l
instance HLength l (HSucc HZero) => HSingleton l

hSingle :: (HSingleton l, HHead l e) => l -> e
hSingle = hHead


{-----------------------------------------------------------------------------}
