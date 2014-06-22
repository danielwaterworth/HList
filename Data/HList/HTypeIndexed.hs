
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed operations on typeful heterogeneous lists.
-}

module Data.HList.HTypeIndexed where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HList
import Data.HList.HArray
import Data.HList.HOccurs ()

-- --------------------------------------------------------------------------
-- | Map a type to a natural (index within the collection)
-- This is a purely type-level computation

instance (HEq e1 e b, HType2HNatCase b e1 l n) => HType2HNat e1 (e ': l) n

-- | Helper class
class HType2HNatCase (b :: Bool) (e :: *) (l :: [*]) (n :: HNat) | b e l -> n
instance HOccursNot e l => HType2HNatCase True e l HZero
instance HType2HNat e l n => HType2HNatCase False e l (HSucc n)

hType2HNat :: HType2HNat e l n => Proxy e -> proxy l -> Proxy n
hType2HNat _ _ = Proxy

-- | And lift to the list of types

instance HTypes2HNats ('[] :: [*]) (l :: [*]) '[]

instance (HType2HNat e l n, HTypes2HNats es l ns)
      => HTypes2HNats (e ': es) (l :: [*]) (n ': ns)

hTypes2HNats :: HTypes2HNats es l ns => 
		Proxy (es :: [*]) -> HList l -> Proxy (ns :: [HNat])
hTypes2HNats _ _ = Proxy

-- --------------------------------------------------------------------------
-- Implementing the generic interfaces

instance HDeleteMany e (HList '[]) (HList '[]) where
  hDeleteMany _ HNil = HNil

instance (HEq e1 e b, HDeleteManyCase b e1 e l l1)
      => HDeleteMany e1 (HList (e ': l)) (HList l1) where
  hDeleteMany p (HCons e l) = 
      hDeleteManyCase (Proxy :: Proxy b) p e l

class HDeleteManyCase (b :: Bool) e1 e l l1 | b e1 e l -> l1 where
  hDeleteManyCase :: Proxy b -> Proxy e1 -> e -> HList l -> HList l1

instance HDeleteMany e (HList l) (HList l1) => HDeleteManyCase True e e l l1
 where
  hDeleteManyCase _ p _ l = hDeleteMany p l


instance HDeleteMany e1 (HList l) (HList l1)
      => HDeleteManyCase False e1 e l (e ': l1) where
  hDeleteManyCase _ p e l = HCons e (hDeleteMany p l)

-- --------------------------------------------------------------------------
-- Type-indexed operations in terms of the natural-based primitives

hDeleteAt :: forall e l n. (HDeleteAtHNat n l, HType2HNat e l n) => 
	   Proxy e -> HList l -> HList (HDeleteAtHNatR n l)
hDeleteAt _p l = hDeleteAtHNat (Proxy :: Proxy n) l

hUpdateAt :: forall n e l.
		 (HUpdateAtHNat n e l, HType2HNat e l n) => 
		 e -> HList l -> (HList (HUpdateAtHNatR n e l))
hUpdateAt e l = hUpdateAtHNat (Proxy :: Proxy n) e l

hProjectBy :: forall (ns :: [HNat]) (ps :: [*]) (l :: [*]).
	      (HProjectByHNatsCtx ns l, HTypes2HNats ps l ns,
	      ps ~ (HProjectByHNatsR ns l)) =>
	      Proxy ps -> HList l -> HList ps
hProjectBy _ps l = hProjectByHNats (Proxy ::Proxy ns) l

hSplitBy :: forall (ps :: [*]) l ns.
	    (HProjectByHNatsCtx ns l, HProjectAwayByHNatsCtx ns l,
	     HTypes2HNats ps l ns) =>
	    Proxy ps -> HList l -> (HList (HProjectByHNatsR ns l), 
				    HList (HProjectAwayByHNatsR ns l))
hSplitBy _ps l = hSplitByHNats (Proxy ::Proxy ns) l


-- | should this instead delete the first element of that type?
instance (HDeleteAtHNat n l, HType2HNat e l n, l' ~ HDeleteAtHNatR n l)
      => HDeleteAtLabel HList e l l' where
    hDeleteAtLabel _ = hDeleteAtHNat (Proxy :: Proxy n)
