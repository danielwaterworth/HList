{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic cast
   and TTypeable-based type equality. This module is prepared for use
   with Hugs. This model works in principle also for GHC (see module
   GHCTTypeable) perhaps modulo some slight differences in test cases.

-}


import Datatypes1
import FakePrelude hiding (TypeCast,typeCast)
import HListPrelude
import HOccurs
import TypeCastGeneric2

type Animal =  HCons Key
	(HCons Name
	(HCons Breed
	(HCons Price
	HNil)))

angus :: Animal
angus =  HCons (Key 42)
	(HCons (Name "Angus")
	(HCons  Cow
	(HCons (Price 75.5)
	HNil)))

{-----------------------------------------------------------------------------}

class HList l => HLookup e l where
	hLookup :: l -> e
instance TypeCast e e' => HLookup e' (HCons e HNil) where
	hLookup (HCons e _) = typeCast e
instance HLookup' e (HCons e' (HCons x l)) => HLookup e (HCons e' (HCons x l)) where
	hLookup l = hLookup' l
class HList l => HLookup' e l where
	hLookup' :: l -> e
instance (HList l,HOccursNot e l) => HLookup' e (HCons e l) where
	hLookup' (HCons e _) = e
instance HLookup' e l => HLookup' e (HCons e' l) where
	hLookup' (HCons _ l) = hLookup' l

{-----------------------------------------------------------------------------}

class (HList l,HList l') => HReverseAcc l l' l'' | l l' -> l'' where
	hReverseAcc :: l -> l' -> l''
instance HList l => HReverseAcc HNil l l where
	hReverseAcc _ l = l
instance (HList l',HReverseAcc l (HCons a l') l'') => HReverseAcc (HCons a l) l' l'' where
	hReverseAcc (HCons a l) l' = hReverseAcc l (HCons a l')

{-----------------------------------------------------------------------------}

class HDeleteByProxy' l e HNil l' => HDeleteByProxy l e l' where
	hDeleteByProxy :: l -> Proxy e -> l'
instance HDeleteByProxy' l e HNil l' => HDeleteByProxy l e l' where
	hDeleteByProxy l e = hDeleteByProxy' l e HNil
class HList l => HDeleteByProxy' l e acc l' where
	hDeleteByProxy' :: l -> Proxy e -> acc -> l'
instance (HList (HCons e l),HReverseAcc acc l l') => HDeleteByProxy' (HCons e l) e acc l' where
	hDeleteByProxy' (HCons _ l) _ acc = hReverseAcc acc l
instance (HDeleteByProxy' l e (HCons e' acc) l') => HDeleteByProxy' (HCons e' l) e acc l' where
	hDeleteByProxy' (HCons e' l) e acc = hDeleteByProxy' l e (HCons e' acc)

{-----------------------------------------------------------------------------}

class (HLookup x l,HDeleteByProxy l x m,HLookup y m) => Tuple l m x y where
	tuple :: l -> (m,(x,y))
instance (HLookup x l,HDeleteByProxy l x m,HLookup y m) => Tuple l m x y where
	tuple l = let
		x = hLookup l
		m = hDeleteByProxy l (toProxy x)
		y = hLookup m
		in (m,(x,y))

{-----------------------------------------------------------------------------}

main = print "OK"

{-----------------------------------------------------------------------------}
