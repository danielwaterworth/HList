{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module TIP where

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.

-}


import FakePrelude
import HListPrelude
import HListGoodies
import HArray
import HOccurs
import HTypeIndexed


{-----------------------------------------------------------------------------}

-- The newtype for type-indexed products

newtype TIP l = TIP l
        deriving (Read,Show)

mkTIP :: HTypeIndexed l => l -> TIP l
mkTIP = TIP

unTIP :: TIP l -> l
unTIP (TIP l) = l

emptyTIP :: TIP HNil
emptyTIP = mkTIP HNil



{-----------------------------------------------------------------------------}

-- Type-indexed type sequences

class HList l => HTypeIndexed l
instance HTypeIndexed HNil
instance (HFreeType e l,HTypeIndexed l) => HTypeIndexed (HCons e l)


{-----------------------------------------------------------------------------}

-- HFreeType lifted to TIPs

instance HFreeType p l => HFreeType p (TIP l)


{-----------------------------------------------------------------------------}

-- Type-indexed extension

hExtend' e (TIP l) = mkTIP (HCons e l)

{-

Valid type I

hExtend' :: (HTypeIndexed l, HFreeType (HProxy e) l)
         => e -> TIP l -> TIP (HCons e l)  

Valid type II

*TIP> :t hExtend'
hExtend' :: forall l e.
            (HTypeIndexed (HCons e l)) =>
            e -> TIP l -> TIP (HCons e l)

-}


{-----------------------------------------------------------------------------}

-- Lift extension through HExtend

instance HTypeIndexed (HCons e l)
      => HExtend e (TIP l) (TIP (HCons e l))
 where
  hExtend = hExtend'


{-----------------------------------------------------------------------------}

-- Lifting previous operations -----------------------------------------------


instance ( HAppend l l' l''
         , HTypeIndexed l''
         ) 
           => HAppend (TIP l) (TIP l') (TIP l'')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')


instance ( HQualify u a q
         , HTypeIndexed u
         , HTypeIndexed q
         ) 
           => HQualify (TIP u) a (TIP q)
 where
  hQualify   (TIP u) a = mkTIP (hQualify u a)
  hUnqualify (TIP q) a = mkTIP (hUnqualify q a)


instance HOccursMany e l
      => HOccursMany e (TIP l)
 where
  hOccursMany = hOccursMany . unTIP


instance HOccursMany1 e l
      => HOccursMany1 e (TIP l)
 where
  hOccursMany1 = hOccursMany1 . unTIP


instance HOccursFst e l
      => HOccursFst e (TIP l)
 where
  hOccursFst = hOccursFst . unTIP


instance HOccursOpt e l
      => HOccursOpt e (TIP l)
 where
  hOccursOpt = hOccursOpt . unTIP


instance HOccurs e l
      => HOccurs e (TIP l)
 where
  hOccurs = hOccurs . unTIP


instance HOccursGrounded e l
      => HOccursGrounded e (TIP l)
 where
  hOccursGrounded = hOccursGrounded . unTIP


instance ( HDeleteByProxy l e l'
         , HTypeIndexed l'
         )
           => HDeleteByProxy (TIP l) e (TIP l')
 where
  hDeleteByProxy (TIP l) p = TIP (hDeleteByProxy l p)


instance ( HUpdateByType l e
         , HTypeIndexed l
         )
           => HUpdateByType (TIP l) e
 where
  hUpdateByType (TIP l) e = mkTIP (hUpdateByType l e)


instance ( HProjectByProxies l ps l'
         , HTypeIndexed l'
         )
           => HProjectByProxies (TIP l) ps (TIP l')
 where
  hProjectByProxies (TIP l) ps
   =
     mkTIP (hProjectByProxies l ps)


instance ( HSplitByProxies l ps l' l''
         , HTypeIndexed l'
         , HTypeIndexed l''
         )
           => HSplitByProxies (TIP l) ps (TIP l') (TIP l'')
 where
  hSplitByProxies (TIP l)
   =
     (\(x,y) -> (mkTIP x,mkTIP y)) . hSplitByProxies l


{-----------------------------------------------------------------------------}

-- Complement of the HFreeType class

class HBoundType e r
instance HBoundType e (HCons e l)
instance HBoundType e l => HBoundType e (HCons e' l)


-- Class to compute a Boolean for "free type" status

class HBool b => HFreeTypeStatus e l b | e l -> b
class (HBool b, HBool b') => HFreeTypeStatus' b e l b' | b e l -> b'
instance HFreeTypeStatus e HNil HTrue
instance ( TypeEqBool e e' b
         , HFreeTypeStatus' b e l b'
         )
      =>   HFreeTypeStatus e (HCons e' l) b'
instance HFreeTypeStatus' HTrue e l HFalse
instance HFreeTypeStatus e l b
      => HFreeTypeStatus' HFalse e l b


{-----------------------------------------------------------------------------}

-- Subtyping for TIPs

instance SubType (TIP l) (TIP HNil)
instance (HBoundType e l, SubType (TIP l) (TIP l'))
      =>  SubType (TIP l) (TIP (HCons e l'))


{-----------------------------------------------------------------------------}

-- Sample code

{-

Assume

myTipyCow = TIP myAnimal

animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
animalKey = hOccurs

Session log

*TIP> :t myTipyCow
myTipyCow :: TIP Animal

*TIP> hOccurs myTipyCow :: Breed
Cow

*TIP> hExtend BSE myTipyCow
TIP (HCons BSE 
    (HCons (Key 42)
    (HCons (Name "Angus")
    (HCons Cow
    (HCons (Price 75.5)
     HNil)))))

*TIP> BSE .*. myTipyCow
--- same as before ---

*TIP> Sheep .*. myTipyCow
Type error ...

*TIP> Sheep .*. hDelete myTipyCow (HProxy::HProxy Breed)
TIP (HCons Sheep (HCons (Key 42) (HCons (Name "Angus") (HCons (Price 75.5) HNil))))

*TIP> hUpdate myTipyCow Sheep
TIP (HCons (Key 42) (HCons (Name "Angus") (HCons Sheep (HCons (Price 75.5) HNil))))

-}

{-----------------------------------------------------------------------------}
