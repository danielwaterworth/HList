{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Result-type-driven operations on typeful heterogeneous lists.
-}

module Data.HList.HOccurs (
    module Data.HList.HOccurs,
    ) where

import Data.HList.FakePrelude
import Data.HList.HListPrelude

-- --------------------------------------------------------------------------
-- Given an HList l and an element type e return the suffix of l
-- whose head has the type e. Return HNil if l does not have
-- an element of type e.

class HOccurrence e l l' | e l -> l' where
    hOccurrence :: e -> l -> l'

instance HOccurrence e HNil HNil where
    hOccurrence _ = id

instance (TypeEq e e' b, HOccurrence' b e (HCons e' l) l')
    => HOccurrence e (HCons e' l) l' where
    hOccurrence = hOccurrence' (undefined::b)

class HOccurrence' b e l l' | b e l -> l' where
    hOccurrence' :: b -> e -> l -> l'

instance HOccurrence' HTrue e (HCons e l) (HCons e l) where
    hOccurrence' _ _ = id

instance HOccurrence e l l' => HOccurrence' HFalse e (HCons e' l) l' where
    hOccurrence' _ e (HCons _ l) = hOccurrence e l


-- --------------------------------------------------------------------------
-- Zero or more occurrences

class HOccursMany e l where
  hOccursMany :: l -> [e]

instance (HOccurrence e l l', HOccursMany' e l') 
    => HOccursMany e l
 where
  hOccursMany l = hOccursMany' (hOccurrence (undefined::e) l)

class HOccursMany' e l where
  hOccursMany' :: l -> [e]

instance HOccursMany' e HNil where
  hOccursMany' _ = []

instance (e ~ e', HOccursMany e l) => HOccursMany' e (HCons e' l) where
  hOccursMany' (HCons e l) = e : hOccursMany l


-- --------------------------------------------------------------------------
-- One or more occurrences

hOccursMany1 :: forall e l l'.
		(HOccurrence e l (HCons e l'), HOccursMany e l') =>
		l -> (e,[e])
hOccursMany1 l = let (HCons e l') = hOccurrence (undefined::e) l in
		 (e,hOccursMany (l'::l'))

-- --------------------------------------------------------------------------
-- The first occurrence

hOccursFst :: HOccurrence e l (HCons e l') => l -> e
hOccursFst l = let (HCons e _) = hOccurrence e l in e


-- --------------------------------------------------------------------------
-- One occurrence and nothing is left
-- This constraint is used in many places

class HOccurs e l where
  hOccurs :: l -> e

data TypeNotFound e

instance Fail (TypeNotFound e) => HOccurs e HNil where
    hOccurs = undefined

instance (HOccurrence e (HCons x y) l', HOccurs' e l') 
    => HOccurs e (HCons x y) where
    hOccurs = hOccurs' . hOccurrence (undefined::e)

class HOccurs' e l where
    hOccurs' :: l -> e

instance Fail (TypeNotFound e) => HOccurs' e HNil where
    hOccurs' = undefined

instance (e ~ e', HOccursNot e l) => HOccurs' e (HCons e' l) where
    hOccurs' (HCons e _) = e


-- --------------------------------------------------------------------------
-- Zero or at least one occurrence

hOccursOpt :: forall e l l'. 
	      (HOccurrence e l l', HOccursOpt' e l') => l -> Maybe e
hOccursOpt = hOccursOpt' . hOccurrence (undefined::e)

class HOccursOpt' e l where
  hOccursOpt' :: l -> Maybe e

instance HOccursOpt' e HNil where
  hOccursOpt' _ = Nothing

instance e ~ e' => HOccursOpt' e (HCons e' l) where
  hOccursOpt' (HCons e _) = Just e

-- --------------------------------------------------------------------------
-- Class to test that a type is "free" in a type sequence

data TypeFound e
class HOccursNot e l
instance HOccursNot e HNil
instance (TypeEq e e' b, HOccursNot' b e l) => HOccursNot e (HCons e' l)
class HOccursNot' b e l
instance Fail (TypeFound e) => HOccursNot' HTrue e l
instance HOccursNot' HFalse e HNil
instance HOccursNot e (HCons e' l) => HOccursNot' HFalse e (HCons e' l)


-- --------------------------------------------------------------------------

class HProject l l'
 where
  hProject :: l -> l'

instance HProject l HNil
 where
  hProject _ = HNil

instance ( HList l'
         , HOccurs e l
         , HProject l l'
         )
      =>   HProject l (HCons e l')
 where
  hProject l = HCons (hOccurs l) (hProject l)


-- --------------------------------------------------------------------------

-- * Illustration of typical test scenarios
{- $example

Retrieve the Breed of an animal.

> ghci-or-hugs> hOccurs myAnimal :: Breed
> Cow


Normal hOccurs requires specification of the result type even if the result
type is determined by the fact that we are faced with a singleton list.

> ghci-or-hugs> hOccurs (HCons 1 HNil)
>
> <interactive>:1:
>     No instance for (HOccurs e1 (HCons e HNil))


However, hOccurs can be elaborated as improved as follows:

> ghci-or-hugs> hLookup (HCons 1 HNil)
> 1

-}

