
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
import Data.HList.HList

-- --------------------------------------------------------------------------
-- Given an HList l and an element type e return the suffix of l
-- whose head has the type e. Return HNil if l does not have
-- an element of type e.

class HOccurrence (e1 :: *) (l :: [*]) (l' :: [*]) | e1 l -> l' where
    hOccurrence :: Proxy e1 -> HList l -> HList l'

instance HOccurrence e1 '[] '[] where
    hOccurrence _ = id

instance (HEq e1 e b, HOccurrence' b e1 (e ': l) l')
    => HOccurrence e1 (e ': l) l' where
    hOccurrence = hOccurrence' (Proxy::Proxy b)

class HOccurrence' (b :: Bool) (e1 :: *) (l :: [*]) (l' :: [*]) | b e1 l -> l' where
    hOccurrence' :: Proxy b -> Proxy e1 -> HList l -> HList l'

instance HOccurrence' True e1 (e ': l) (e ': l) where
    hOccurrence' _ _ = id

instance HOccurrence e1 l l' => HOccurrence' False e1 (e ': l) l' where
    hOccurrence' _ e (HCons _ l) = hOccurrence e l


-- --------------------------------------------------------------------------
-- Zero or more occurrences

class HOccursMany e (l :: [*]) where
  hOccursMany :: HList l -> [e]

instance (HOccurrence e l l', HOccursMany' e l') 
    => HOccursMany e l
 where
  hOccursMany l = hOccursMany' (hOccurrence (Proxy::Proxy e) l)

class HOccursMany' e l where
  hOccursMany' :: HList l -> [e]

instance HOccursMany' e '[] where
  hOccursMany' _ = []

instance (e ~ e1, HOccursMany e l) => HOccursMany' e (e1 ': l) where
  hOccursMany' (HCons e l) = e : hOccursMany l


-- --------------------------------------------------------------------------
-- One or more occurrences

hOccursMany1 :: forall e l l'.
		(HOccurrence e l (e ': l'), HOccursMany e l') =>
		HList l -> (e,[e])
hOccursMany1 l = case hOccurrence (Proxy :: Proxy e) l of
		   (HCons e l') -> (e,hOccursMany (l'::HList l'))

-- --------------------------------------------------------------------------
-- The first occurrence

hOccursFst :: forall e l l'. HOccurrence e l (e ': l') => HList l -> e
hOccursFst l = case hOccurrence (Proxy::Proxy e) l of HCons e _ -> e

-- --------------------------------------------------------------------------
-- One occurrence and nothing is left
-- This constraint is used in many places

data TypeNotFound e

instance (HOccurrence e (x ': y) l', HOccurs' e l')
    => HOccurs e (HList (x ': y)) where
    hOccurs = hOccurs' . hOccurrence (Proxy ::Proxy e)

class HOccurs' e l where
    hOccurs' :: HList l -> e

instance Fail (TypeNotFound e) => HOccurs' e '[] where
    hOccurs' = error "Data.HList.FakePrelude.Fail must have no instances"

instance (e ~ e1, HOccursNot e l) => HOccurs' e (e ': l) where
    hOccurs' (HCons e _) = e


-- --------------------------------------------------------------------------
-- Zero or at least one occurrence

hOccursOpt :: forall e l l'. 
	      (HOccurrence e l l', HOccursOpt' e l') => HList l -> Maybe e
hOccursOpt = hOccursOpt' . hOccurrence (Proxy :: Proxy e)

class HOccursOpt' e l where
  hOccursOpt' :: HList l -> Maybe e

instance HOccursOpt' e '[] where
  hOccursOpt' _ = Nothing

instance e ~ e1 => HOccursOpt' e (e1 ': l) where
  hOccursOpt' (HCons e _) = Just e

-- --------------------------------------------------------------------------
-- Class to test that a type is "free" in a type sequence

data TypeFound e
instance HOccursNot e ('[]::[*])
instance (HEq e e1 b, HOccursNot' b e l) => HOccursNot e (e1 ': l)
class HOccursNot' (b :: Bool) e (l :: [*])
instance Fail (TypeFound e) => HOccursNot' True e l
instance HOccursNot e l => HOccursNot' False e l


-- --------------------------------------------------------------------------

instance HProject (HList l) (HList '[]) where
  hProject _ = HNil

instance (HOccurs e l, HProject l (HList l'))
      =>   HProject l (HList (e ': l'))
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
