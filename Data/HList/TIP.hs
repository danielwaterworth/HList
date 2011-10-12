{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
    FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.
-}

module Data.HList.TIP where


import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HArray
import Data.HList.HOccurs
import Data.HList.HTypeIndexed


-- --------------------------------------------------------------------------
-- | The newtype for type-indexed products

newtype TIP l = TIP l deriving (Read,Show)

mkTIP :: HTypeIndexed l => l -> TIP l
mkTIP = TIP

unTIP :: TIP l -> l
unTIP (TIP l) = l

emptyTIP :: TIP HNil
emptyTIP = mkTIP HNil



-- --------------------------------------------------------------------------
-- | Type-indexed type sequences

class HList l => HTypeIndexed l
instance HTypeIndexed HNil
instance (HOccursNot e l,HTypeIndexed l) => HTypeIndexed (HCons e l)


-- --------------------------------------------------------------------------
--
-- One occurrence and nothing is left
--
-- This variation provides an extra feature for singleton lists.
-- That is, the result type is unified with the element in the list.
-- Hence the explicit provision of a result type can be omitted.
--

instance e' ~ e => HOccurs e (TIP (HCons e' HNil))
 where
  hOccurs (TIP (HCons e' _)) = e'

instance HOccurs e (HCons x (HCons y l))
      => HOccurs e (TIP (HCons x (HCons y l)))
 where
  hOccurs (TIP l) = hOccurs l


-- --------------------------------------------------------------------------
-- HOccursNot lifted to TIPs

instance HOccursNot e l => HOccursNot e (TIP l)


-- --------------------------------------------------------------------------

-- | Type-indexed extension
--
-- signature is inferred
--
-- > hExtend' :: (HTypeIndexed t, HOccursNot e t) => e -> TIP t -> TIP (HCons e t)
hExtend' e (TIP l) = mkTIP (HCons e l)

{- $example

Valid type I

hExtend' :: (HTypeIndexed l, HOccursNot e l)
         => e -> TIP l -> TIP (HCons e l)

Valid type II

*TIP> :t hExtend'
hExtend' :: forall l e.
            (HTypeIndexed (HCons e l)) =>
            e -> TIP l -> TIP (HCons e l)

-}


-- --------------------------------------------------------------------------
-- Lift extension through HExtend

instance ( HOccursNot e l
         , HTypeIndexed l
         )
      => HExtend e (TIP l) (TIP (HCons e l))
 where
  hExtend = hExtend'


-- --------------------------------------------------------------------------
-- Lifting previous operations


instance ( HAppend l l' l''
         , HTypeIndexed l''
         )
           => HAppend (TIP l) (TIP l') (TIP l'')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')


instance HOccurrence e l l' => HOccurrence e (TIP l) l'
 where
  hOccurrence e = hOccurrence e . unTIP


-- --------------------------------------------------------------------------
-- | Shielding type-indexed operations
-- The absence of signatures is deliberate! They all must be inferred.

onTIP f (TIP l) = mkTIP (f l)

tipyDelete  p t  = onTIP (hDeleteAtProxy p) t
tipyUpdate  e t  = onTIP (hUpdateAtType e) t
tipyProject ps t = onTIP (hProjectByProxies ps) t


-- Split produces two TIPs
tipySplit ps (TIP l) = (mkTIP l',mkTIP l'')
 where
  (l',l'') = hSplitByProxies ps l


-- --------------------------------------------------------------------------

-- Subtyping for TIPs

instance SubType (TIP l) (TIP HNil)
instance (HOccurs e l, SubType (TIP l) (TIP l'))
      =>  SubType (TIP l) (TIP (HCons e l'))


-- --------------------------------------------------------------------------

-- * Sample code

{- $sampleCode

Assume

> myTipyCow = TIP myAnimal

> animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
> animalKey = hOccurs

Session log

> *TIP> :t myTipyCow
> myTipyCow :: TIP Animal

> *TIP> hOccurs myTipyCow :: Breed
> Cow

> *TIP> hExtend BSE myTipyCow
> TIP (HCons BSE
>     (HCons (Key 42)
>     (HCons (Name "Angus")
>     (HCons Cow
>     (HCons (Price 75.5)
>      HNil)))))

> *TIP> BSE .*. myTipyCow
> --- same as before ---

> *TIP> Sheep .*. myTipyCow
> Type error ...

> *TIP> Sheep .*. tipyDelete myTipyCow (HProxy::HProxy Breed)
>TIP (HCons Sheep (HCons (Key 42) (HCons (Name "Angus") (HCons (Price 75.5) HNil))))

> *TIP> tipyUpdate myTipyCow Sheep
> TIP (HCons (Key 42) (HCons (Name "Angus") (HCons Sheep (HCons (Price 75.5) HNil))))

-}



