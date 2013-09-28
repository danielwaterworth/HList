{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-dodgy-exports #-}

{- |
   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records

   The three-ish models of labels that go with this module;

   * "Data.HList.Label3"

   * "Data.HList.Label6"

   * "Data.HList.MakeLabels"


   These used to work:

   * "Data.HList.Label1"

   * "Data.HList.Label2"

   * "Data.HList.Label4"

   * "Data.HList.Label5"

-}

module Data.HList.Record
(
    -- * Records

    -- ** Labels
    -- $labels
    LVPair(..),
    labelLVPair,
    newLVPair,
    (.=.), (:=:), (.-.),

    -- ** Record
    Record(..),
    mkRecord,
    emptyRecord,

    -- *** Getting Labels
    RecordLabels,
    recordLabels,

    -- *** Getting Values
    RecordValues(..),
    recordValues,

    -- * Operations
    -- ** Show
    -- | A corresponding 'Show' instance exists as
    --
    -- > show x = "Record {" ++ showComponents "" x ++ "}"
    ShowComponents(..),
    ShowLabel(..),

    -- ** Lookup
    HasField(..),
    HasField'(..),
    (.!.),

    -- ** Delete
    -- | 'hDeleteAtLabel' @label record@
    hDeleteAtLabel,

    -- ** Update
    -- | 'hUpdateAtLabel' @label value record@
    hUpdateAtLabel,
    hTPupdateAtLabel,
    (.@.), (.<.),

    -- ** Rename Label
    hRenameLabel,

    -- ** Projection
    -- $projection
    hProjectByLabels,
    hProjectByLabels2,

   {-
    -- ** Unions
    -- *** Left
    HLeftUnion(hLeftUnion),
    HLeftUnionBool(hLeftUnionBool),
    (.<++.),

    -- *** Symmetric
    -- $symmetricUnion
    UnionSymRec(unionSR),


    -- ** Reorder Labels
    hRearrange,
    -}

    -- ** Extension
    -- | 'hExtend', 'hAppend'
    (.*.),

    -- * Unclassified
    -- | Probably internals, that may not be useful

    DuplicatedLabel(..),
    -- ExtraField(..),
    -- FieldNotFound(..),
    H2ProjectByLabels(h2projectByLabels),
    H2ProjectByLabels'(h2projectByLabels'),
    HLabelSet,
    HLabelSet',
    HRLabelSet,
    HRLabelSet',
    -- HRearrange(hRearrange2),
    -- HRearrange'(hRearrange2'),
    -- UnionSymRec'(..)
    module Data.HList.Record, -- if something was forgotten
) where


import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HList
import Data.HList.HArray


-- --------------------------------------------------------------------------

-- $labels Record types as label-value pairs, where label is purely phantom.
-- Thus the run-time representation of a field is the same as that of
-- its value, and the record, at run-time, is indistinguishable from
-- the HList of field values. At run-time, all information about the
-- labels is erased.

-- | Field of label l with value type v
-- Polykinded with respect to l: label may be a symbol, a nat, etc.
newtype LVPair l v = LVPair { valueLVPair :: v } deriving Eq

-- | Label accessor
labelLVPair :: LVPair l v -> Label l
labelLVPair = undefined

newLVPair :: Label l -> v -> LVPair l v
newLVPair _ = LVPair

infixr 4 :=:
type l :=: v = LVPair l v


infixr 4 .=.
{-|

  Create a value with the given label. Analagous to a data
  constructor such as 'Just', 'Left', or 'Right'. Higher fixity
  than record-modification operations like (.*.), (.-.), etc. to
  support expression like the below w/o parentheses:

  > label1 .=. value1 .*.
  > label2 .=. value2 .*.
  > emptyRecord

-}
(.=.) :: Label l -> v -> LVPair l v
l .=. v = newLVPair l v


newtype Record (r :: [*]) = Record (HList r) -- deriving Eq


-- | Build a record
mkRecord :: HRLabelSet r => HList r -> Record r
mkRecord = Record


-- | Build an empty record
emptyRecord :: Record '[]
emptyRecord = mkRecord HNil


-- | Propery of a proper label set for a record: no duplication of labels

data DuplicatedLabel l

class HRLabelSet (ps :: [*])
instance HRLabelSet '[]
instance HRLabelSet '[x]
instance ( HEq l1 l2 leq
         , HRLabelSet' l1 l2 leq r
         ) => HRLabelSet (LVPair l1 v1 ': LVPair l2 v2 ': r)

class HRLabelSet' l1 l2 (leq::Bool) (r :: [*])
instance ( HRLabelSet (LVPair l2 () ': r)
         , HRLabelSet (LVPair l1 () ': r)
         ) => HRLabelSet' l1 l2 False r
instance ( Fail (DuplicatedLabel l1) ) => HRLabelSet' l1 l2 True r


-- Relation between HLabelSet and HRLabelSet
{-
instance (HZip ls vs ps, HLabelSet ls) => HRLabelSet ps
-}

class HLabelSet ls
instance HLabelSet '[]
instance HLabelSet '[x]
instance ( HEq l1 l2 leq
         , HLabelSet' l1 l2 leq r
         ) => HLabelSet (l1 ': l2 ': r)

class HLabelSet' l1 l2 (leq::Bool) r
instance ( HLabelSet (l2 ': r)
         , HLabelSet (l1 ': r)
         ) => HLabelSet' l1 l2 False r
instance ( Fail (DuplicatedLabel l1) ) => HLabelSet' l1 l2 True r

-- | Construct the (phantom) list of labels of the record.
--

type family RecordLabels (r :: [*]) :: [k]
type instance RecordLabels '[]               = '[]
type instance RecordLabels (LVPair l v ': r) = l ': RecordLabels r

recordLabels :: Record r -> Proxy (RecordLabels r)
recordLabels = undefined

-- | Construct the HList of values of the record.
class RecordValues (r :: [*]) where
  type RecordValuesR r :: [*]
  recordValues' :: HList r -> HList (RecordValuesR r)

instance RecordValues '[] where
  type RecordValuesR '[] = '[]
  recordValues' _ = HNil
instance RecordValues r=> RecordValues (LVPair l v ': r) where
   type RecordValuesR (LVPair l v ': r) = v ': RecordValuesR r
   recordValues' (HCons (LVPair v) r) = HCons v (recordValues' r)

recordValues :: RecordValues r => Record r -> HList (RecordValuesR r)
recordValues (Record r) = recordValues' r


-- Polykinded
type family LabelsOf (ls :: [*]) :: [*]
type instance LabelsOf '[] = '[]
type instance LabelsOf (Label l ': r)  = l ': LabelsOf r

hLabels :: HList l -> Proxy (LabelsOf l)
hLabels = undefined

-- --------------------------------------------------------------------------

-- 'Show' instance to appeal to normal records

instance ShowComponents r => Show (Record r) where
  show (Record r) =  "Record{"
                  ++ showComponents "" r
                  ++ "}"

class ShowComponents l where
  showComponents :: String -> HList l -> String

instance ShowComponents '[] where
  showComponents _ _ = ""

instance ( ShowLabel l
         , Show v
         , ShowComponents r
         )
      =>   ShowComponents (LVPair l v ': r) where
  showComponents comma (HCons f@(LVPair v) r)
     =  comma
     ++ showLabel ((labelLVPair f) :: Label l)
     ++ "="
     ++ show v
     ++ showComponents "," r

class ShowLabel l where
  showLabel :: Label l -> String



-- --------------------------------------------------------------------------

-- Extension

instance HRLabelSet (LVPair l v ': r) 
    => HExtend (LVPair (l :: k) v) (Record r) where
  type HExtendR (LVPair l v) (Record r) = Record (LVPair l v ': r)
  f .*. (Record r) = mkRecord (HCons f r)


-- * For records

{-|

  [@(.*.)@]
           Add a field to a record. Analagous to (++) for
           lists.

  > record .*. field1
  >        .*. field2

-}

-- --------------------------------------------------------------------------

-- Concatenation

instance (HRLabelSet (HAppendList r1 r2), HAppend (HList r1) (HList r2))
    => HAppend (Record r1) (Record r2) where
  type HAppendR (Record r1) (Record r2) = Record (HAppendList r1 r2)
  hAppend (Record r) (Record r') = mkRecord (hAppend r r')

-- --------------------------------------------------------------------------

-- Lookup
--
-- |
-- This is a baseline implementation.
-- We use a helper class, HasField, to abstract from the implementation.

-- | Because 'hLookupByLabel' is so frequent and important, we implement
-- it separately, more efficiently. The algorithm is familiar assq, only
-- the comparison operation is done at compile-time
class HasField (l::k) r v | l r -> v where
    hLookupByLabel:: Label l -> r -> v

{-
instance ( RecordLabels r ls
         , HFind l ls n
         , HLookupByHNat n r (LVPair l v)
         ) => HasField l (Record r) v
  where
    hLookupByLabel l (Record r) = v
      where
        ls = recordLabels' r
        n = hFind l ls
        (LVPair v) = hLookupByHNat n r

-}


instance (HEq l l1 b, HasField' b l (LVPair l1 v1 ': r) v)
    => HasField l (Record (LVPair l1 v1 ': r)) v where
    hLookupByLabel l (Record r) =
             hLookupByLabel' (undefined::Proxy b) l r

-- XXX Alas, I have to set l :: * even though it could be other kinds.
-- Check back with GHC 7.8

class HasField' (b::Bool) (l :: k) (r::[*]) v | b l r -> v where
    hLookupByLabel':: Proxy b -> Label l -> HList r -> v

instance HasField' True l (LVPair l v ': r) v where
    hLookupByLabel' _ _ (HCons (LVPair v) _) = v
instance HasField l (Record r) v => HasField' False l (fld ': r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l (Record r)



infixr 9 .!.
{-|
  Lookup a value in a record, by its label. Analagous to (!!), the
  list indexing operation. Highest fixity, like (!!).

  > record1 .*. label1 .=. record2 .!. label1
  >         .*. label2 .=. record2 .!. label2

-}
(.!.) :: (HasField l r v) => r -> Label l -> v
r .!. l =  hLookupByLabel l r

-- --------------------------------------------------------------------------

-- Delete

hDeleteAtLabel :: forall l t t1 t2. 
   (H2ProjectByLabels '[l] t t1 t2) =>
   Label l -> Record t -> Record t2
hDeleteAtLabel _ (Record r) = 
  Record $ snd $ h2projectByLabels (undefined::Proxy '[l]) r

infixl 2 .-.
{-|
  Remove a field from a record. At the same
  level as other record modification options (.*.). Analagous
  to (\\) in lists.

  > record1 .-. label1

  > label1 .=. value1 .*.
  > label2 .=. value2 .-.
  > label2 .*.
  > emptyRecord

  > label1 .=. value1 .-.
  > label1 .*.
  > label2 .=. value2 .*.
  > emptyRecord

  > record1 .*. label1 .=. record2 .!. label1
  >         .*. label2 .=. record2 .!. label2
  >         .-. label1

-}
(.-.) :: (H2ProjectByLabels '[l] r _r' r') =>
    Record r -> Label l -> Record r'
r .-. l =  hDeleteAtLabel l r


-- --------------------------------------------------------------------------

-- Update

hUpdateAtLabel :: forall (r :: [*]) (l :: *) (n::HNat) (v :: *). 
  (HFind l (RecordLabels r) n, HUpdateAtHNat n (LVPair l v) r) =>
  Label l -> v -> Record r -> Record (HUpdateAtHNatR n (LVPair l v) r)
hUpdateAtLabel l v (Record r) = 
    Record (hUpdateAtHNat (undefined::Proxy n) (newLVPair l v) r)

infixr 2 .@.
{-|

  Update a field with a particular value.
  Same fixity as (.*.) so that extensions and updates can be chained.
  There is no real list analogue, since there is no Prelude defined
  update.

  > label1 .=. value1 .@. record1

-}
f@(LVPair v) .@. r  =  hUpdateAtLabel (labelLVPair f) v r


-- --------------------------------------------------------------------------
-- Projection

-- $projection
-- It is also an important operation: the basis of many
-- deconstructors -- so we try to implement it efficiently.


-- | @hProjectByLabels ls r@ returns @r@ with only the labels in @ls@ remaining
hProjectByLabels :: (HRLabelSet a, H2ProjectByLabels ls t a b) => 
	Proxy ls -> Record t -> Record a
hProjectByLabels ls (Record r) = mkRecord (fst $ h2projectByLabels ls r)

-- | See 'H2ProjectByLabels'
hProjectByLabels2 :: 
    (H2ProjectByLabels ls t t1 t2, HRLabelSet t1, HRLabelSet t2) =>
    Proxy ls -> Record t -> (Record t1, Record t2)
hProjectByLabels2 ls (Record r) = (mkRecord rin, mkRecord rout)
   where (rin,rout) = h2projectByLabels ls r

-- | /Invariant/:
--
--  > r === rin `disjoint-union` rout
--  > labels rin === ls
--  >     where (rin,rout) = hProjectByLabels ls r
-- XXX ls should be [k], poly-kinded; OTH, r, rin, rout are of kind [*]
class H2ProjectByLabels (ls::[*]) r rin rout | ls r -> rin rout where
    h2projectByLabels :: Proxy ls -> HList r -> (HList rin,HList rout)

instance H2ProjectByLabels '[] r '[] r where
    h2projectByLabels _ r = (HNil,r)

instance H2ProjectByLabels (l ': ls) '[] '[] '[] where
    h2projectByLabels _ _ = (HNil,HNil)

instance (HMemberM l1 (l ': ls) b,
          H2ProjectByLabels' b (l ': ls) (LVPair l1 v1 ': r1) rin rout)
    => H2ProjectByLabels (l ': ls) (LVPair l1 v1 ': r1) rin rout where
    h2projectByLabels = h2projectByLabels' (undefined::(Proxy b))

class H2ProjectByLabels' (b::Maybe [*]) (ls::[*]) r rin rout 
                         | b ls r -> rin rout where
    h2projectByLabels' :: Proxy b -> Proxy ls -> 
				     HList r -> (HList rin,HList rout)

instance H2ProjectByLabels ls1 r rin rout =>
    H2ProjectByLabels' ('Just ls1) ls (f ': r) (f ': rin) rout where
    h2projectByLabels' _ _ (HCons x r) = (HCons x rin, rout)
        where (rin,rout) = h2projectByLabels (undefined::Proxy ls1) r

instance H2ProjectByLabels ls r rin rout =>
    H2ProjectByLabels' 'Nothing ls (f ': r) rin (f ': rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
        where (rin,rout) = h2projectByLabels ls r

-- --------------------------------------------------------------------------
-- | Rename the label of record

{-
hRenameLabel :: (HRLabelSet (HCons (LVPair l v) t2),HasField e t1 v,H2ProjectByLabels (HCons e HNil) t1 t t2) =>
    e -> l -> Record t1 -> Record (LVPair l v ': t2)
-}
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteAtLabel l r
  r'' = newLVPair l' v .*. r'


-- --------------------------------------------------------------------------

-- | A variation on 'hUpdateAtLabel': type-preserving update.
hTPupdateAtLabel l v r = hUpdateAtLabel l v r
 where
   te :: a -> a -> ()
   te _ _ = ()
   _ = te v (hLookupByLabel l r)

{- ^

We could also say:

> hTPupdateAtLabel l v r = hUpdateAtLabel l v r `asTypeOf` r

Then we were taking a dependency on Haskell's type equivalence.
This would also constrain the actual implementation of hUpdateAtLabel.

-}

infixr 2 .<.
{-|
  Another variation on update, so give it the same fixity as (.\@.).

-}
f@(LVPair v) .<. r = hTPupdateAtLabel (labelLVPair f) v r

-- --------------------------------------------------------------------------
-- | Subtyping for records

instance H2ProjectByLabels (RecordLabels r2) r1 r2 rout
    => SubType (Record r1) (Record r2)


{-
-- --------------------------------------------------------------------------

-- Left Union

class  HLeftUnion r r' r'' | r r' -> r''
 where hLeftUnion :: r -> r' -> r''

instance HLeftUnion r (Record HNil) r
 where   hLeftUnion r _ = r

instance ( RecordLabels r ls
         , HMember l ls b
         , HLeftUnionBool b r (LVPair l v) r'''
         , HLeftUnion (Record r''') (Record r') r''
         )
           => HLeftUnion (Record r) (Record (HCons (LVPair l v) r')) r''
  where
   hLeftUnion (Record r) (Record (HCons f r')) = r''
    where
     b       = hMember (labelLVPair f) (recordLabels' r)
     r'''    = hLeftUnionBool b r f
     r''     = hLeftUnion (Record r''') (Record r')

class  HLeftUnionBool b r f r' | b r f -> r'
 where hLeftUnionBool :: b -> r -> f -> r'

instance HLeftUnionBool HTrue r f r
   where hLeftUnionBool _ r _  = r

instance HLeftUnionBool HFalse r f (HCons f r)
   where hLeftUnionBool _ r f = HCons f r

infixl 1 .<++.
{-|
  Similar to list append, so give this slightly lower fixity than
  (.*.), so we can write:

   > field1 .=. value .*. record1 .<++. record2

-}
(.<++.) ::  (HLeftUnion r r' r'') => r -> r' -> r''
r .<++. r' = hLeftUnion r r'


-- --------------------------------------------------------------------------
-- $symmetricUnion
-- Compute the symmetric union of two records r1 and r2 and
-- return the pair of records injected into the union (ru1, ru2).
--
-- To be more precise, we compute the symmetric union /type/ @ru@
-- of two record /types/ @r1@ and @r2@. The emphasis on types is important.
--
-- The two records (ru1,ru2) in the result of 'unionSR' have the same
-- type ru, but they are generally different values.
-- Here the simple example: suppose
--
-- >  r1 = (Label .=. True)  .*. emptyRecord
-- >  r2 = (Label .=. False) .*. emptyRecord
--
-- Then 'unionSR' r1 r2 will return (r1,r2). Both components of the result
-- are different records of the same type.
--
--
-- To project from the union ru, use 'hProjectByLabels'.
-- It is possible to project from the union obtaining a record
-- that was not used at all when creating the union.
--
-- We do assure however that if @unionSR r1 r2@ gave @(r1u,r2u)@,
-- then projecting r1u onto the type of r1 gives the /value/ identical
-- to r1. Ditto for r2.

class UnionSymRec r1 r2 ru | r1 r2 -> ru where
    unionSR :: r1 -> r2 -> (ru, ru)

instance UnionSymRec r1 (Record HNil) r1 where
    unionSR r1 _ = (r1, r1)

instance ( RecordLabels r1 ls
         , HMember l ls b
         , UnionSymRec' b (Record r1) (LVPair l v) (Record r2') ru
         )
    => UnionSymRec (Record r1) (Record (HCons (LVPair l v) r2')) ru
    where
    unionSR r1 (Record (HCons f r2')) =
        unionSR' (undefined::b) r1 f (Record r2')

class UnionSymRec' b r1 f2 r2' ru | b r1 f2 r2' -> ru where
    unionSR' :: b -> r1 -> f2 -> r2'  -> (ru, ru)

-- | Field f2 is already in r1, so it will be in the union of r1
-- with the rest of r2.
--
-- To inject (HCons f2 r2) in that union, we should replace the
-- field f2
instance (UnionSymRec r1 r2' (Record ru),
          HasField l2 ru v2,
          HUpdateAtHNat n (LVPair l2 v2) ru ru,
          RecordLabels ru ls,
          HFind l2 ls n)
    => UnionSymRec' HTrue r1 (LVPair l2 v2) r2' (Record ru) where
    unionSR' _ r1 (LVPair v2) r2' = (ul, ur')
       where (ul,ur) = unionSR r1 r2'
             ur' = hTPupdateAtLabel (undefined::l2) v2 ur


instance (UnionSymRec r1 r2' (Record ru),
          HExtend f2 (Record ru) (Record (HCons f2 ru)))
    => UnionSymRec' HFalse r1 f2 r2' (Record (HCons f2 ru)) where
    unionSR' _ r1 f2 r2' = (ul', ur')
       where (ul,ur) = unionSR r1 r2'
             ul' = hExtend f2 ul
             ur' = hExtend f2 ur

-- --------------------------------------------------------------------------
-- | Rearranges a record by labels. Returns the record r, rearranged such that
-- the labels are in the order given by ls. (recordLabels r) must be a
-- permutation of ls.
hRearrange :: (HLabelSet ls, HRearrange ls r r') => ls -> Record r -> Record r'
hRearrange ls (Record r) = Record $ hRearrange2 ls r

-- | Helper class for 'hRearrange'
class HRearrange ls r r' | ls r -> r' where
    hRearrange2 :: ls -> r -> r'

instance HRearrange HNil HNil HNil where
   hRearrange2 _ _ = HNil

instance (H2ProjectByLabels (HCons l HNil) r rin rout,
          HRearrange' l ls rin rout r') =>
        HRearrange (HCons l ls) r r' where
   hRearrange2 ~(HCons l ls) r = hRearrange2' l ls rin rout
      where (rin, rout) = h2projectByLabels (HCons l HNil) r

-- | Helper class 2 for 'hRearrange'
class HRearrange' l ls rin rout r' | l ls rin rout -> r' where
    hRearrange2' :: l -> ls -> rin -> rout -> r'
instance HRearrange ls rout r' =>
        HRearrange' l ls (HCons (LVPair l v) HNil) rout (HCons (LVPair l v) r') where
   hRearrange2' _ ls (HCons lv@(LVPair v) HNil) rout = HCons (LVPair v `asTypeOf` lv) (hRearrange2 ls rout)

data ExtraField l = ExtraField
data FieldNotFound l = FieldNotFound

-- | For improved error messages
instance Fail (FieldNotFound l) => 
        HRearrange' l ls HNil rout (FieldNotFound l) where
   hRearrange2' _ _ _ _ = FieldNotFound

-- | For improved error messages
instance Fail (ExtraField l) => 
          HRearrange HNil (HCons (LVPair l v) a) (ExtraField l) where
   hRearrange2 _ _ = ExtraField
-}
