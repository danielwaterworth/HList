{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Basic declarations for typeful heterogeneous lists.

 -}

 
module HListPrelude where

import FakePrelude

{-----------------------------------------------------------------------------}

-- Heterogeneous type sequences

data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)


{-----------------------------------------------------------------------------}

-- The set of all types of heterogeneous lists

class HList l
instance HList HNil
instance HList l => HList (HCons e l)


{-----------------------------------------------------------------------------}

-- Public constructor

hNil  :: HNil
hNil  =  HNil

hCons :: HList l => e -> l -> HCons e l
hCons e l = HCons e l


{-----------------------------------------------------------------------------}

-- Basic list functions

class HHead l h | l -> h
 where
  hHead :: l -> h

instance HHead (HCons e l) e
 where
  hHead (HCons e _) = e

class HTail l l' | l -> l'
 where
  hTail :: l -> l'

instance HTail (HCons e l) l
 where
  hTail (HCons _ l) = l



{-----------------------------------------------------------------------------}

-- A class for extension

class HExtend e l l' | e l -> l', l' -> e l
 where
  hExtend :: e -> l -> l'

instance HExtend e HNil (HCons e HNil)
 where
  hExtend e l = HCons e l

instance HList l => HExtend e (HCons e' l) (HCons e (HCons e' l))
 where
  hExtend e l = HCons e l


{-----------------------------------------------------------------------------}

-- Appending HLists

-- The normal append for comparison

append :: [a] -> [a] -> [a]
append [] l = l
append (x:l) l' = x : append l l'


-- The class HAppend

class HAppend l l' l'' | l l' -> l''
 where
  hAppend :: l -> l' -> l''


-- The instance following the normal append

instance HList l => HAppend HNil l l
 where
  hAppend HNil l = l

instance (HAppend l l' l'', HList l'')
      => HAppend (HCons x l) l' (HCons x l'')
 where
  hAppend (HCons x l) l' = hCons x (hAppend l l')

   
{-----------------------------------------------------------------------------}

-- Reversing HLists

class HReverse l1 l2 | l1 -> l2, l2 -> l1
 where
  hReverse:: l1 -> l2

instance (HReverse' HNil l2 l3, HReverse' HNil l3 l2)
      =>  HReverse l2 l3
 where
  hReverse l1 = hReverse' HNil l1


-- l3 = (reverse l2) ++ l1

class HReverse' l1 l2 l3 | l1 l2 -> l3
 where
  hReverse':: l1 -> l2 -> l3
    
instance HReverse' l1 HNil l1
 where
  hReverse' l1 HNil = l1
    
instance HReverse' (HCons a l1) l2' l3
      => HReverse' l1 (HCons a l2') l3
 where
  hReverse' l1 (HCons a l2') = hReverse' (HCons a l1) l2'


-- Naive HReverse

class NaiveHReverse l l' | l -> l'
 where
  naiveHReverse :: l -> l'

instance NaiveHReverse HNil HNil
 where
  naiveHReverse HNil = HNil

instance ( NaiveHReverse l l'
         , HAppend l' (HCons e HNil) l''
         )
      =>   NaiveHReverse (HCons e l) l''
 where
  naiveHReverse (HCons e l)
   = hAppend (naiveHReverse l) (HCons e HNil)


{-----------------------------------------------------------------------------}

--
-- A nicer notation for lists
--

-- List termination
hEnd t@(HCons x y) = t

{-
   Note: 
        - x :: HCons a b
            means: forall a b. x :: HCons a b
        - hEnd x
            means: exists a b. x :: HCons a b
-}


-- Building non-empty lists

hBuild   :: forall r a. (HBuild' HNil a r) => a -> r
hBuild x =  hBuild' HNil x

class HBuild' l a r | r-> a l
 where
  hBuild' :: l -> a -> r

instance HReverse (HCons a l) (HCons a' l')
      => HBuild' l a (HCons a' l')
 where
  hBuild' l x = hReverse (HCons x l)

instance HBuild' (HCons a l) b r
      => HBuild' l a (b->r)
 where
  hBuild' l x y = hBuild' (HCons x l) y

{-

HList> let x = hBuild True in hEnd x
HCons True HNil

HList> let x = hBuild True 'a' in hEnd x
HCons True (HCons 'a' HNil)

HList> let x = hBuild True 'a' "ok" in hEnd x
HCons True (HCons 'a' (HCons "ok" HNil))

HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5))
HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))

HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5)) == myAnimal
True

-}

{-----------------------------------------------------------------------------}

-- A heterogeneous apply operator

class HApply f a r | f a -> r
 where
  hApply :: f -> a -> r


-- Normal function application

instance HApply (x -> y) x y
 where
  hApply f x = f x


-- Identity

data Id = Id

instance HApply Id x x
 where
  hApply _ x = x


{-----------------------------------------------------------------------------}

-- A heterogeneous fold for all types

class HList l => HFoldr f v l r | f v l -> r
 where
  hFoldr :: f -> v -> l -> r

instance HFoldr f v HNil v
 where
  hFoldr _ v _ = v

instance ( HFoldr f v l r
         , HApply f (e,r) r'
         )
      => HFoldr f v (HCons e l) r'
 where
  hFoldr f v (HCons e l) = hApply f (e,hFoldr f v l)


{-----------------------------------------------------------------------------}

-- Map a heterogeneous list to a homogeneous one

class HMapOut f r e
 where
  hMapOut :: f -> r -> [e]

instance HMapOut f HNil e
 where
  hMapOut _ _ = []

instance ( HMapOut f l e'
         , HApply f e e'
         )
      =>   HMapOut f (HCons e l) e'
 where
  hMapOut f (HCons e l) = hApply f e : hMapOut f l


{-----------------------------------------------------------------------------}

-- A reconstruction of append

append' :: [a] -> [a] -> [a]
append' l l' = foldr (:) l' l

hAppend' l l' = hFoldr ApplyHCons l' l

data ApplyHCons = ApplyHCons

instance HList l => HApply ApplyHCons (e,l) (HCons e l)
 where
  hApply ApplyHCons (e,l) = hCons e l


{-----------------------------------------------------------------------------}

-- A heterogeneous map for all types

class HList l => HMap f l l' | f l -> l'
 where
  hmap :: f -> l -> l'

instance HMap f HNil HNil
 where
  hmap _ HNil = hNil

instance ( HApply f e e'
         , HMap f l l'
         , HList l
         , HList l'
         )
      => HMap f (HCons e l) (HCons e' l')
 where
  hmap f (HCons e l) = hCons (hApply f e) (hmap f l)


{-----------------------------------------------------------------------------}

-- A function for showing

data HShow  = HShow
data HSeq x = HSeq x

instance Show x => HApply HShow x (IO ())
 where
  hApply _ x = do putStrLn $ show x

instance ( Monad m 
         , HApply f x (m ())
         )
      => HApply (HSeq f) (x,m ()) (m ())
 where
  hApply (HSeq f) (x,c) = do hApply f x; c


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

HListPrelude> hSplit (hFlag (HCons "1" HNil))
(HCons "1" HNil,HNil)
HListPrelude> hSplit $ hFlag (HCons "1" HNil)
ERROR - Unresolved overloading
*** Type       : (HSplit HNil a b, HCons' [Char] a c) => (c,b)
*** Expression : hSplit $ hFlag (HCons "1" HNil)

-}


{-----------------------------------------------------------------------------}
