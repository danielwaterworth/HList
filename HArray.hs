{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Array-like access to HLists.

 -}

 
module HArray where

import FakePrelude
import HList


{-----------------------------------------------------------------------------}

-- A lookup operation

class HLookupByHNat l i e | l i -> e
 where
  hLookupByHNat :: l -> i -> e

instance HList l => HLookupByHNat (HCons e l) HZero e
 where
  hLookupByHNat (HCons e _) _ = e

instance (HList l, HNat n, HLookupByHNat l n e')
      => HLookupByHNat (HCons e l) (HSucc n) e'
 where
  hLookupByHNat (HCons _ l) (HSucc n) = hLookupByHNat l n


{-----------------------------------------------------------------------------}

-- A delete operation

class HDeleteByHNat l i l' | l i -> l'
 where
  hDeleteByHNat :: l -> i -> l'

instance HList l => HDeleteByHNat (HCons e l) HZero l
 where
  hDeleteByHNat (HCons _ l) HZero = l

instance ( HDeleteByHNat l n l'
         , HList l
         , HList l'
         , HNat n
         )
           => HDeleteByHNat (HCons e l) (HSucc n) (HCons e l')
 where
  hDeleteByHNat (HCons e l) (HSucc n) = hCons e (hDeleteByHNat l n)


{-----------------------------------------------------------------------------}

-- An update operation

class HUpdateByHNat l i e l' | l i e -> l'
 where
  hUpdateByHNat :: l -> i -> e -> l'

instance HList l => HUpdateByHNat (HCons e l) HZero e' (HCons e' l)
 where
  hUpdateByHNat (HCons e l) _ e' = HCons e' l

instance (HList l, HList l', HNat n, HUpdateByHNat l n e' l')
      => HUpdateByHNat (HCons e l) (HSucc n) e' (HCons e l')
 where
  hUpdateByHNat (HCons e l) (HSucc n) e'
               = HCons e (hUpdateByHNat l n e')


{-----------------------------------------------------------------------------}

-- Projection as iterated deletion

hSplitByHNats l il = (l'',l''')
-- (ly,ln)
 where
    l'         = toHJust l
    (l'',l''') = hSplitByHNats' l' il
--    ly         = fromHJust l''
--    ln         = fromHJust l'''

class HSplitByHNats' l il l' l'' | l il -> l' l''
 where
  hSplitByHNats' :: l -> il -> (l',l'')

instance HSplitByHNats' l HNil HNil l
 where
  hSplitByHNats' l HNil = (HNil,l)

instance ( HLookupByHNat l i e
         , HUpdateByHNat l i HNothing l'
         , HSplitByHNats' l' il l'' l'''
         )
      =>   HSplitByHNats' l (HCons i il) (HCons e l'') l'''
 where
  hSplitByHNats' l (HCons i il) = (HCons e l'',l''')
   where
    e          = hLookupByHNat l i
    l'         = hUpdateByHNat l i HNothing
    (l'',l''') = hSplitByHNats' l' il


{-----------------------------------------------------------------------------}

-- Turn list in a list of justs

class ToHJust l l' | l -> l'
 where 
  toHJust :: l -> l'

instance ToHJust HNil HNil
 where
  toHJust HNil = HNil

instance ToHJust l l' => ToHJust (HCons e l) (HCons (HJust e) l')
 where
  toHJust (HCons e l) = HCons (HJust e) (toHJust l)


{-----------------------------------------------------------------------------}

-- Extract justs from list of maybes

class FromHJust l l' | l -> l'
 where
  fromHJust :: l -> l'

instance FromHJust HNil HNil
 where
  fromHJust HNil = HNil

instance FromHJust l l' => FromHJust (HCons HNothing l) l'
 where
  fromHJust (HCons _ l) = fromHJust l

instance FromHJust l l' => FromHJust (HCons (HJust e) l) (HCons e l')
 where
  fromHJust (HCons (HJust e) l) = HCons e (fromHJust l)


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
         , HList il
         , HList l
         , HList l'
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
         , HNat len
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

class HNat x => HBetween x y | x -> y
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
         , HList x
         , HList z
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
         , HList l
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

instance ( HLength l n
         , HList l
         , HNat n
         )
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

-- Samples

myProj1 = hProjectByHNats myAnimal (HCons HZero (HCons HZero HNil))
myProj2 = hProjectByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))
myProj3 = hProjectAwayByHNats myAnimal (HCons HZero HNil)
myProj4 = hSplitByHNats myAnimal (HCons HZero (HCons (HSucc HZero) HNil))

{-

*HArray> myProj1
HCons (Key 42) (HCons (Key 42) HNil)

*HArray> myProj2
HCons (Key 42) (HCons Cow HNil)

*HArray> myProj3
HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil))

*HArray> myProj4
(HCons (Key 42) (HCons (Name "Angus") HNil),HCons Cow (HCons (Price 75.5) HNil)

-}

-- A test tuple for main files
testHArray = (myProj1,myProj2,myProj3,myProj4)

{-----------------------------------------------------------------------------}
