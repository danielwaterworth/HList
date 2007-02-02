{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records

   The are different models of labels that go with this module;
   see the files Label?.hs.

-}

 
module Record where

import FakePrelude
import HListPrelude
import HArray


{-----------------------------------------------------------------------------}

-- Record types as label-value pairs, where label is purely phantom.
-- Thus the run-time representation of a field is the same as that of
-- its value, and the record, at run-time, is indistinguishable from
-- the HList of field values. At run-time, all information about the
-- labels is erased.

newtype F l v = F v			-- Field of label l with value type v
labelF :: F l v -> l; labelF = undefined  -- label accessor
newF :: l -> v -> F l v
newF _ = F

newtype Record r = Record r


-- Build a record

mkRecord :: HRLabelSet r => r -> Record r
mkRecord = Record


-- Build an empty record

emptyRecord = mkRecord HNil


-- Propery of a proper label set for a record: no duplication of labels
 
class HRLabelSet ps
instance HRLabelSet HNil
instance HRLabelSet (HCons x HNil)
instance (HEq l1 l2 HFalse, 
	  HRLabelSet (HCons (F l2 v2) r),
	  HRLabelSet (HCons (F l1 v1) r))
    => HRLabelSet (HCons (F l1 v1) (HCons (F l2 v2) r))
{-
instance (HZip ls vs ps, HLabelSet ls) => HRLabelSet ps
-}

class HLabelSet ls
instance HLabelSet HNil
instance (HMember x ls HFalse, HLabelSet ls)
      =>  HLabelSet (HCons x ls)


-- Construct the (phantom) list of labels of the record.
-- This is a type-level only function
class RecordLabels r ls | r -> ls
instance RecordLabels HNil HNil
instance RecordLabels r' ls => RecordLabels (HCons (F l v) r') (HCons l ls)

recordLabels :: RecordLabels r ls => r -> ls
recordLabels = undefined


{-----------------------------------------------------------------------------}

-- A Show instance to appeal to normal records

instance ShowComponents r => Show (Record r)
 where
  show (Record r) =  "Record{"
                  ++ showComponents "" r
                  ++ "}"

class ShowComponents l
 where
  showComponents :: String -> l -> String

instance ShowComponents HNil
 where
  showComponents _ HNil = ""

instance ( ShowLabel l
         , Show v
         , ShowComponents r
         )
      =>   ShowComponents (HCons (F l v) r)
 where
  showComponents comma (HCons f@(F v) r)
     =  comma
     ++ showLabel (labelF f)
     ++ "="
     ++ show v
     ++ showComponents "," r

class ShowLabel l
 where
  showLabel :: l -> String


{-----------------------------------------------------------------------------}

-- Extension for records

instance HRLabelSet (HCons (F l v) r)
    => HExtend (F l v) (Record r) (Record (HCons (F l v) r))
 where
  hExtend f (Record r) = mkRecord (HCons f r)


{-----------------------------------------------------------------------------}

-- Record concatenation

instance ( HRLabelSet r''
	 , HAppend r r' r''
         )
    => HAppend (Record r) (Record r') (Record r'')
 where
  hAppend (Record r) (Record r') = mkRecord (hAppend r r')


{-----------------------------------------------------------------------------}

-- Lookup operation

{- 
   The first version:
hLookupByLabel l (Record r) = v
 where
   (ls,vs) = hUnzip r
   n       = hFind l ls
   v       = hLookupByHNat n vs

-}


-- Because hLookupByLabel is so frequent and important, we
-- implement it separately. The algorithm is familiar assq,
-- only the comparison operation is done at compile-time

class HasField l r v | l r -> v where
    hLookupByLabel:: l -> r -> v

instance HasField l r v => HasField l (Record r) v where
    hLookupByLabel l (Record r) = hLookupByLabel l r

class HasField' b l r v | b l r -> v where
    hLookupByLabel':: b -> l -> r -> v

instance (HEq l l' b, HasField' b l (HCons (F l' v') r) v)
    => HasField l (HCons (F l' v') r) v where
    hLookupByLabel l r@(HCons f' _) = hLookupByLabel' (hEq l (labelF f')) l r

instance HasField' HTrue l (HCons (F l v) r) v where 
    hLookupByLabel' _ _ (HCons (F v) _) = v
instance HasField l r v => HasField' HFalse l (HCons fld r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l r



{-----------------------------------------------------------------------------}

-- Delete operation

hDeleteAtLabel l (Record r) = Record r'
 where
  (_,r')  = h2projectByLabels (HCons l HNil) r


{-----------------------------------------------------------------------------}

-- Update operation

hUpdateAtLabel l v (Record r) = Record r'
 where
  n    = hFind l (recordLabels r)
  r'   = hUpdateAtHNat n (newF l v) r


{-----------------------------------------------------------------------------}
-- Projection for records
-- It is also an important operation: the basis of many
-- deconstructors -- so we try to implement it efficiently.

hProjectByLabels ls (Record r)
 = 
   mkRecord (fst $ h2projectByLabels ls r)

-- Invariant: r = rin `disjoint-union` rout
--            labels(rin) = ls
class H2ProjectByLabels ls r rin rout | ls r -> rin rout where
    h2projectByLabels :: ls -> r -> (rin,rout)

instance H2ProjectByLabels ls HNil HNil HNil where
    h2projectByLabels _ _ = (HNil,HNil)

instance (HMember l' ls b, 
	  H2ProjectByLabels' b ls (HCons (F l' v') r') rin rout)
    => H2ProjectByLabels ls (HCons (F l' v') r') rin rout where
    -- h2projectByLabels = h2projectByLabels' (undefined::b)
    -- The latter is solely for the Hugs benefit
    h2projectByLabels ls r@(HCons f' _) = h2projectByLabels' b ls r
      where b = hMember (labelF f') ls

class H2ProjectByLabels' b ls r rin rout | b ls r -> rin rout where
    h2projectByLabels' :: b -> ls -> r -> (rin,rout)
    
instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HTrue ls (HCons f' r') (HCons f' rin) rout where
    h2projectByLabels' _ ls (HCons x r) = (HCons x rin, rout)
	where (rin,rout) = h2projectByLabels ls r

instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HFalse ls (HCons f' r') rin (HCons f' rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
	where (rin,rout) = h2projectByLabels ls r


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteAtLabel l r
  r'' = hExtend (newF l' v) r'


{-----------------------------------------------------------------------------}

-- A variation on update: type-preserving update.

hTPupdateAtLabel l v r = hUpdateAtLabel l v r
 where
   te :: a -> a -> ()
   te _ _ = ()
   _ = te v (hLookupByLabel l r)

{-

-- We could also say:

hTPupdateAtLabel l (v::v) r = hUpdateAtLabel l v r `asTypeOf` r

-- Then we were taking a dependency on Haskell's type equivalence.
-- This would also constrain the actual implementation of hUpdateAtLabel.

-}

{-----------------------------------------------------------------------------}

-- Subtyping for records

instance ( RecordLabels r' ls
         , H2ProjectByLabels ls r r' rout
         )
    => SubType (Record r) (Record r')


{-----------------------------------------------------------------------------}

class  HLeftUnion r r' r'' | r r' -> r''
 where hLeftUnion :: r -> r' -> r''

instance HLeftUnion r (Record HNil) r
 where   hLeftUnion r _ = r

instance ( RecordLabels r ls
         , HMember l ls b
         , HLeftUnionBool b r (F l v) r'''
         , HLeftUnion (Record r''') (Record r') r''
         )
           => HLeftUnion (Record r) (Record (HCons (F l v) r')) r''
  where
   hLeftUnion (Record r) (Record (HCons f r')) = r''
    where
     b       = hMember (labelF f) (recordLabels r)
     r'''    = hLeftUnionBool b r f
     r''     = hLeftUnion (Record r''') (Record r')

class  HLeftUnionBool b r f r' | b r f -> r'
 where hLeftUnionBool :: b -> r -> f -> r'

instance HLeftUnionBool HTrue r f r
   where hLeftUnionBool _ r _  = r

instance HLeftUnionBool HFalse r f (HCons f r)
   where hLeftUnionBool _ r f = HCons f r


{-----------------------------------------------------------------------------}
