{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Result-type-driven operations on typeful heterogeneous lists.

-}


module HOccurs where

import FakePrelude
import HListPrelude
import HListGoodies
import HArray


{-----------------------------------------------------------------------------}

-- Zero or more occurrences

class HOccursMany e l
 where
  hOccursMany :: l -> [e]

instance HOccursMany e HNil
 where
  hOccursMany HNil = []

instance ( HOccursMany e l, HList l )
      =>   HOccursMany e (HCons e l)
 where
  hOccursMany (HCons e l) = e:hOccursMany l

instance ( HOccursMany e l, HList l )
      =>   HOccursMany e (HCons e' l)
 where
  hOccursMany (HCons _ l) = hOccursMany l


{-----------------------------------------------------------------------------}

-- One or more occurrences

class HOccursMany1 e l
 where
  hOccursMany1 :: l -> (e,[e])

instance ( HOccursMany e l, HList l )
      =>   HOccursMany1 e (HCons e l)
 where
  hOccursMany1 (HCons e l) = (e,hOccursMany l)

instance ( HOccursMany1 e l, HList l )
      => HOccursMany1 e (HCons e' l)
 where
  hOccursMany1 (HCons _ l) = hOccursMany1 l


{-----------------------------------------------------------------------------}

-- The first occurrence

class HOccursFst e l
 where
  hOccursFst :: l -> e

instance HList l
      => HOccursFst e (HCons e l)
 where
  hOccursFst (HCons e l) = e

instance ( HOccursFst e l, HList l )
      =>   HOccursFst e (HCons e' l)
 where
  hOccursFst (HCons _ l) = hOccursFst l


{-----------------------------------------------------------------------------}

-- One occurrence and nothing is left

class HOccurs e l
 where
  hOccurs :: l -> e

instance ( HList l
         , HFreeType e l
         )
           => HOccurs e (HCons e l)
 where
  hOccurs (HCons e _) = e

instance ( HOccurs e l
         , HList l
         )
           => HOccurs e (HCons e' l)
 where
  hOccurs (HCons _ l) = hOccurs l


{-----------------------------------------------------------------------------}

-- One occurrence and nothing is left
-- This variation even grounds the result type for a singleton list.

class HOccursGrounded e l
 where
  hOccursGrounded :: l -> e

instance TypeUnify e e'
      => HOccursGrounded e' (HCons e HNil)
 where
  hOccursGrounded (HCons e _) = typeUnify e

instance HOccursGrounded' e (HCons x l)
      => HOccursGrounded e (HCons x l)
 where
  hOccursGrounded l = hOccursGrounded' l

class HOccursGrounded' e l
 where
  hOccursGrounded' :: l -> e

instance HFreeType e l
      => HOccursGrounded' e (HCons e l)
 where
  hOccursGrounded' (HCons e _) = e

instance HOccursGrounded' e l
      => HOccursGrounded' e (HCons e' l)
 where
  hOccursGrounded' (HCons _ l) = hOccursGrounded' l


{-----------------------------------------------------------------------------}

-- Zero or at least one occurrence

class HOccursOpt e l
 where
  hOccursOpt :: l -> Maybe e

instance HOccursOpt e HNil
 where
  hOccursOpt HNil = Nothing

instance HOccursOpt e (HCons e l)
 where
  hOccursOpt (HCons e l) = Just e

instance HOccursOpt e l
      => HOccursOpt e (HCons e' l)
 where
  hOccursOpt (HCons _ l) = hOccursOpt l


{-----------------------------------------------------------------------------}

-- Class to test that a type is "free" in a type sequence

class HFreeType e l
instance HFreeType e HNil
instance (TypeNotEq e e', HFreeType e l)
      =>  HFreeType e (HCons e' l)


{-----------------------------------------------------------------------------}

-- Illustration of typical test scenarios

{-

Retrieve the Breed of an animal.

ghci-or-hugs> hOccurs myAnimal :: Breed
Cow

-}

{-

Normal hOccurs cannot ground result type even if it is imaginable.

ghci-or-hugs> hOccurs (HCons 1 HNil)

<interactive>:1:
    No instance for (HOccurs e1 (HCons e HNil))

-}

{-

hOccurs can be elaborated to ground the result type for singletons.

ghci-or-hugs> hOccursGrounded (HCons 1 HNil)
1

-}

{-----------------------------------------------------------------------------}
