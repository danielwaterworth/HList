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

-- Convenience notation

(!)   :: HLookup l x y => l -> x -> y
l ! x =  hLookup l x


{-----------------------------------------------------------------------------}

-- A lookup operation

class HLookup l i e | l i -> e
 where
  hLookup :: l -> i -> e

instance HList l => HLookup (HCons e l) HZero e
 where
  hLookup (HCons e _) _ = e

instance (HList l, HNat n, HLookup l n e')
      => HLookup (HCons e l) (HSucc n) e'
 where
  hLookup (HCons _ l) (HSucc n) = hLookup l n


{-----------------------------------------------------------------------------}

-- A delete operation

class HDelete l i l' | l i -> l'
 where
  hDelete :: l -> i -> l'

instance HList l => HDelete (HCons e l) HZero l
 where
  hDelete (HCons _ l) HZero = l

instance ( HDelete l n l'
         , HList l
         , HList l'
         , HNat n
         )
           => HDelete (HCons e l) (HSucc n) (HCons e l')
 where
  hDelete (HCons e l) (HSucc n) = hCons e (hDelete l n)


{-----------------------------------------------------------------------------}

-- Type-preserving update

class HUpdateTP l i e | l i -> e
 where
  hUpdateTP :: l -> i -> e -> l

instance HList l => HUpdateTP (HCons e l) HZero e
 where
  hUpdateTP (HCons e l) _ e' = HCons e' l

instance (HList l, HNat n, HUpdateTP l n e')
      => HUpdateTP (HCons e l) (HSucc n) e'
 where
  hUpdateTP (HCons e l) (HSucc n) e'
           = HCons e (hUpdateTP l n e')


{-----------------------------------------------------------------------------}

-- Type-changing update

class HUpdateTC l i e l' | l i e -> l'
 where
  hUpdateTC :: l -> i -> e -> l'

instance HList l => HUpdateTC (HCons e l) HZero e' (HCons e' l)
 where
  hUpdateTC (HCons e l) _ e' = HCons e' l

instance (HList l, HList l', HNat n, HUpdateTC l n e' l')
      => HUpdateTC (HCons e l) (HSucc n) e' (HCons e l')
 where
  hUpdateTC (HCons e l) (HSucc n) e'
           = HCons e (hUpdateTC l n e')


{-----------------------------------------------------------------------------}

-- A combined lookup-delete operation

class HLookDel l i e l' | l i -> e l'
 where
  hLookDel :: l -> i -> (e,l')

instance HList l => HLookDel (HCons e l) HZero e l
 where
  hLookDel (HCons e l) _ = (e,l)

instance ( HLookDel l n e l'
         , HList l
         , HList l'
         , HNat n
         )
           => HLookDel (HCons e' l) (HSucc n) e (HCons e' l')
 where
  hLookDel (HCons e' l) (HSucc n) = (e, hCons e' l')
   where
    (e,l') = hLookDel l n


{-----------------------------------------------------------------------------}

-- Projection as iterated deletion

class HSplit l il l' l'' | l il -> l' l''
 where
  hSplit :: l -> il -> (l',l'')

instance HSplit HNil HNil HNil HNil
 where
  hSplit HNil HNil = (HNil,HNil)

instance ( HExtend e l l'
         , ToHJust l' l''
         , HSplit' l'' il l''' l''''
         , FromHJust l'''  ly
         , FromHJust l'''' ln
         )
      =>   HSplit (HCons e l) il ly ln
 where 
  hSplit (HCons e l) il = (ly,ln)
   where
    l'           = hExtend e l
    l''          = toHJust l'
    (l''',l'''') = hSplit' l'' il
    ly           = fromHJust l'''
    ln           = fromHJust l''''

class HSplit' l il l' l'' | l il -> l' l''
 where
  hSplit' :: l -> il -> (l',l'')

instance HSplit' l HNil HNil l
 where
  hSplit' l HNil = (HNil,l)

instance ( HLookup l i e
         , ToHNothing l i l'
         , HSplit' l' il l'' l'''
         )
      =>   HSplit' l (HCons i il) (HCons e l'') l'''
 where
  hSplit' l (HCons i il) = (HCons e l'',l''')
   where
    e          = hLookup l i
    l'         = toHNothing l i
    (l'',l''') = hSplit' l' il


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

-- Mark an index as nothing

class ToHNothing l i l' | l i -> l'
 where
  toHNothing :: l -> i -> l'

instance ToHNothing (HCons (HJust e) l) HZero (HCons HNothing l)
 where
  toHNothing (HCons (HJust _) l) HZero = HCons HNothing l

instance ToHNothing l n l'
      => ToHNothing (HCons e l) (HSucc n) (HCons e l')
 where
  toHNothing (HCons e l) (HSucc n) = HCons e (toHNothing l n)


{-----------------------------------------------------------------------------}

-- A lookup operation with an observable failure type

class HLookupMaybe l i maybe | l i -> maybe
 where
  hLookupMaybe :: l -> i -> maybe

instance HLookupMaybe HNil n HNothing
 where
  hLookupMaybe _ _ = HNothing

instance HList l => HLookupMaybe (HCons e l) HZero (HJust e)
 where
  hLookupMaybe (HCons e _) _ = HJust e

instance (HList l, HNat n, HLookupMaybe l n e')
      => HLookupMaybe (HCons e l) (HSucc n) e'
 where
  hLookupMaybe (HCons _ l) (HSucc n) = hLookupMaybe l n


{-----------------------------------------------------------------------------}

-- Another projection operation

class HProject l il l' | l il -> l'
 where
  hProject :: l -> il -> l'

instance HProject HNil HNil HNil
 where
  hProject _ _ = HNil

instance HProject (HCons e l) HNil HNil
 where
  hProject _ _ = HNil

instance ( HLookup (HCons e l) i e'
         , HProject (HCons e l) il l'
         , HList il
         , HList l
         , HList l'
         )
         => HProject (HCons e l) (HCons i il) (HCons e' l')
 where
  hProject l (HCons i il) = HCons e' l'
   where e' = hLookup l i
         l' = hProject l il
 

{-----------------------------------------------------------------------------}

-- The complement of projection

class HProjectAway l il l' | l il -> l'
 where
  hProjectAway :: l -> il -> l'

instance ( HLength l len
         , HNat len
         , HBetween len nats
         , HDiff nats il il'
         , HProject l il' l'
         )
           => HProjectAway l il l'
 where
  hProjectAway l il = l'
   where
    len  = hLength l
    nats = hBetween len
    il'  = hDiff nats il
    l'   = hProject l il'


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

myProj1 = hProject myAnimal (HCons HZero (HCons HZero HNil))
myProj2 = hProject myAnimal (HCons HZero (HCons (HSucc (HSucc HZero)) HNil))
myProj3 = hProjectAway myAnimal (HCons HZero HNil)
myProj4 = hSplit myAnimal (HCons HZero (HCons (HSucc HZero) HNil))

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


{-----------------------------------------------------------------------------}
