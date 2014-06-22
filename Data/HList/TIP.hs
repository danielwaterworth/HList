
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.
-}

module Data.HList.TIP
  (module Data.HList.TIPtuple,
   module Data.HList.TIP) where


import Data.HList.HListPrelude
import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.HArray
import Data.HList.HTypeIndexed
import Data.HList.HOccurs -- for doctest
import Data.HList.Record
import Data.HList.TIPtuple
import Data.List (intercalate)

import Data.HList.TypeEqO () -- for doctest

-- --------------------------------------------------------------------------
-- * The newtype for type-indexed products

-- | TIPs are like 'Record', except every element of the list 'l'
-- has type @Tagged e e@
newtype TIP (l :: [*]) = TIP{unTIP:: HList l}

instance HMapOut (HShow `HComp` HUntag) l String => Show (TIP l) where
  showsPrec _ (TIP l) = ("TIP{" ++)
                              . (intercalate "," (hMapOut (HShow `HComp` HUntag) l) ++)
                              . ('}' :)


mkTIP :: HTypeIndexed l => HList l -> TIP l
mkTIP = TIP

emptyTIP :: TIP '[]
emptyTIP = mkTIP HNil

-- --------------------------------------------------------------------------
-- * Type-indexed type sequences

-- | this constraint ensures that a TIP created by 'mkTIP' has no
-- duplicates
class (HAllTaggedEq l, HRLabelSet l) => HTypeIndexed (l :: [*])

instance (HAllTaggedEq l, HRLabelSet l) => HTypeIndexed l

class HAllTaggedEq (l :: [*])
instance HAllTaggedEq '[]
instance (HAllTaggedEq l, tee ~ Tagged e e') => HAllTaggedEq (tee ': l)

-- --------------------------------------------------------------------------
-- Implementing the HListPrelude interface

instance (HRLabelSet (Tagged e e ': l), HTypeIndexed l) => HExtend e (TIP l)
 where
  type HExtendR e (TIP l) = TIP (Tagged e e ': l)
  e .*. TIP l = mkTIP (HCons (Tagged e) l)



instance (e ~ e', HasField e (Record l) e') => HasField e (TIP l) e' where
    hLookupByLabel lab (TIP l) = hLookupByLabel lab (Record l)

-- | One occurrence and nothing is left
--
-- This variation provides an extra feature for singleton lists.
-- That is, the result type is unified with the element in the list.
-- Hence the explicit provision of a result type can be omitted.
--

instance (tee ~ Tagged e e) => HOccurs e (TIP '[tee]) where
  hOccurs (TIP (HCons (Tagged e) _)) = e

instance HasField e (Record (x ': y ': l)) e
      => HOccurs e (TIP (x ': y ': l)) where
  hOccurs (TIP l) = Record l .!. (Label :: Label e)


instance (HAppend (HList l) (HList l'), HTypeIndexed (HAppendList l l'))
           => HAppend (TIP l) (TIP l')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')

type instance HAppendR (TIP l) (TIP l') = TIP (HAppendList l l')


instance HOccurrence HList e l l' => HOccurrence TIP e l l'
 where
  hOccurrence e = TIP . hOccurrence e . unTIP

-- --------------------------------------------------------------------------
-- * Shielding type-indexed operations
-- $note The absence of signatures is deliberate! They all must be inferred.

onTIP f (TIP l) = let Record l' = f (Record l) in mkTIP l'

tipyDelete  p t  = onTIP (hDeleteAtLabel p) t

instance (HDeleteAtLabel Record e v v',
          HTypeIndexed v')
      => HDeleteAtLabel TIP e v v' where
  hDeleteAtLabel e v = onTIP (hDeleteAtLabel e) v


tipyUpdate  e t  = hTPupdateAtLabel (fromValue e) e t
  where fromValue :: e -> Label e
        fromValue _ = Label

instance (HUpdateAtLabel Record e' e r r',
          HTypeIndexed r',
         e ~ e') => HUpdateAtLabel TIP e' e r r' where
  hUpdateAtLabel l e r = onTIP (hUpdateAtLabel l e) r


-- tipyProject ps t = onTIP (hProjectBy ps) t

-- | provides a @Lens' (TIP s) a@. 'hLens'' @:: Label a -> Lens' (TIP s) a@
-- is another option.
tipyLens' f s = tipyLens (isSimple f) s
  where
    isSimple :: (a -> f a) -> (a -> f a)
    isSimple = id

{- | provides a @Lens (TIP s) (TIP t) a b@

When using @set@ (also known as @.~@), 'tipyLens'' can address the
ambiguity as to which field \"a\" should actually be updated.

-}
tipyLens f s = hLens x f s
  where
    x = getA f
    getA :: (a -> f b) -> Label a
    getA _ = Label


-- | Split produces two TIPs
tipySplit ps (TIP l) = (mkTIP l',mkTIP l'')
 where
  (l',l'') = hSplitBy ps l


-- --------------------------------------------------------------------------

-- | Subtyping for TIPs

instance SubType (TIP l) (TIP '[])
instance (HOccurs e (TIP l1), SubType (TIP l1) (TIP l2))
      =>  SubType (TIP l1) (TIP (e ': l2))


-- --------------------------------------------------------------------------

-- * Sample code

{- $setup

[@Assume@]

>>> import Data.HList.FakePrelude

>>> :{
newtype Key    = Key Integer deriving (Show,Eq,Ord)
newtype Name   = Name String deriving (Show,Eq)
data Breed     = Cow | Sheep deriving (Show,Eq)
newtype Price  = Price Float deriving (Show,Eq,Ord)
data Disease   = BSE | FM deriving (Show,Eq)
type Animal =  '[Key,Name,Breed,Price]
:}

>>> :{
let myAnimal :: HList Animal
    myAnimal = hBuild (Key 42) (Name "Angus") Cow (Price 75.5)
    myTipyCow = TIP myAnimal
    animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
    animalKey = hOccurs
:}

-}

{- $sessionlog
[@Session log@]

>>> :t myTipyCow
myTipyCow :: TIP Animal

>>> hOccurs myTipyCow :: Breed
Cow

>>> BSE .*. myTipyCow
TIPH[BSE, Key 42, Name "Angus", Cow, Price 75.5]



>>> Sheep .*. tipyDelete (Label::Label Breed) myTipyCow
TIPH[Sheep, Key 42, Name "Angus", Price 75.5]

>>> tipyUpdate Sheep myTipyCow
TIPH[Key 42, Name "Angus", Sheep, Price 75.5]


>>> tipySplit (Proxy :: Proxy '[Name,Price]) myTipyCow
(TIPH[Name "Angus", Price 75.5],TIPH[Key 42, Cow])


-}


{- $sessionlog2

Don't bother repeating the type error:


>>> import Data.Char
>>> :{
let doctestCleanActual x
      | null x = x
      | otherwise = dropWhile isSpace $ lines x !! 2 ++ "\n"
:}

>>> Sheep .*. myTipyCow
No instance for (Fail (TypeFound Breed))

-}
