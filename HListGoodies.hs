{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A wave of type-level functions involving HLists.
   These are all rather trivial but nice for illustration.
   We note that type-level fold would be used easily in many cases.

 -}

 
module HListGoodies where

import FakePrelude
import HListPrelude


{-----------------------------------------------------------------------------}

-- Staged equality for lists

instance HStagedEq HNil HNil
 where
  hStagedEq _ _ = True
 
instance HStagedEq HNil (HCons e l)
 where
  hStagedEq _ _ = False
 
instance HStagedEq (HCons e l) HNil
 where
  hStagedEq _ _ = False

instance ( TypeEqBool e e' b
         , HStagedEq l l'
         , HStagedEq' b e e'
         )
      =>   HStagedEq (HCons e l) (HCons e' l')
 where
  hStagedEq (HCons e l) (HCons e' l') = (hStagedEq' b e e') && b'
   where
    b  = typeEqBool e e'
    b' = hStagedEq l l'

class HStagedEq' b e e'
 where
  hStagedEq' :: b -> e -> e' -> Bool

instance HStagedEq' HFalse e e'
 where
  hStagedEq' _ _ _ = False

instance Eq e => HStagedEq' HTrue e e
 where
  hStagedEq' _ = (==)


{-----------------------------------------------------------------------------}


-- Ensure a list to contain HNats only

class HList l => HNats l
instance HNats HNil
instance (HNat n, HNats ns) => HNats (HCons n ns)


-- Static set property based on HEq

class HSet l
instance HSet HNil
instance (HMember e l HFalse, HSet l) => HSet (HCons e l)


-- Find an element in a set based on HEq
class HNat n => HFind e l n | e l -> n
 where
  hFind :: e -> l -> n

instance ( HEq e e' b
         , HFind' b e l n
         )
      =>   HFind e (HCons e' l) n
 where
  hFind e (HCons e' l) = n
   where
    b  = hEq e e'
    n  = hFind' b e l

class HNat n => HFind' b e l n | b e l -> n
 where
  hFind' :: b -> e -> l -> n

instance HFind' HTrue e l HZero
 where
  hFind' _ _ _ = HZero

instance HFind e l n
      => HFind' HFalse e l (HSucc n)
 where
  hFind' _ e l = HSucc (hFind e l)


-- Membership test

class HBool b => HMember e l b | e l -> b
instance HMember e HNil HFalse
instance (HEq e e' b, HMember e l b', HOr b b' b'')
      =>  HMember e (HCons e' l) b''


-- Turn a heterogeneous list into a homogeneous one

class HList2List l e
 where
  hList2List :: l -> [e]

instance HList2List HNil e
 where
  hList2List HNil = []

instance HList2List l e
      => HList2List (HCons e l) e
 where
  hList2List (HCons e l) = e:hList2List l


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

-- Qualification of lists

class HQualify u a q | u a -> q, q a -> u
 where
  hQualify   :: u -> a -> q
  hUnqualify :: q -> a -> u
  

instance HQualify HNil a HNil
 where
  hQualify   HNil _ = hNil
  hUnqualify HNil _ = hNil 

instance (HQualify l a l', HList l, HList l') 
      => HQualify (HCons e l) a (HCons (e,a) l')
 where
  hQualify   (HCons e l) a     = hCons (e,a) (hQualify l a)
  hUnqualify (HCons (e,_) l) a = hCons e     (hUnqualify l a)


{-----------------------------------------------------------------------------}

-- Type-level Boolean flags

class HFlag l l' | l -> l'
 where
  hFlag :: l -> l' 

instance HFlag HNil HNil
 where
  hFlag HNil = HNil

instance HFlag l l'
      => HFlag (HCons e l) (HCons (e,HTrue) l')
 where
  hFlag (HCons e l) = HCons (e,HTrue) (hFlag l)


-- Splitting by HTrue and HFalse

class HSplit l l' l'' | l -> l' l''
 where
  hSplit :: l -> (l',l'')

instance HSplit HNil HNil HNil
 where
  hSplit HNil = (HNil,HNil)

instance HSplit l l' l''
      => HSplit (HCons (e,HTrue) l) (HCons e l') l''
 where
  hSplit (HCons (e,_) l) = (HCons e l',l'')
   where
    (l',l'') = hSplit l

instance HSplit l l' l''
      => HSplit (HCons (e,HFalse) l) l' (HCons e l'')
 where
  hSplit (HCons (e,_) l) = (l',HCons e l'')
   where
    (l',l'') = hSplit l

{-

Inlining "$" makes a difference to hugs.

HListGoodies> hSplit (hFlag (HCons "1" HNil))
(HCons "1" HNil,HNil)
HListGoodies> hSplit $ hFlag (HCons "1" HNil)
ERROR - Unresolved overloading
*** Type       : (HSplit HNil a b, HCons' [Char] a c) => (c,b)
*** Expression : hSplit $ hFlag (HCons "1" HNil)

-}


{-----------------------------------------------------------------------------}
