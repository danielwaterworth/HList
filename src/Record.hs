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

newtype LVPair l v = LVPair v		-- Field of label l with value type v
labelLVPair :: LVPair l v -> l; labelLVPair = undefined  -- label accessor
newLVPair :: l -> v -> LVPair l v
newLVPair _ = LVPair

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
instance ( HEq l1 l2 HFalse
	 , HRLabelSet (HCons (LVPair l2 v2) r)
	 , HRLabelSet (HCons (LVPair l1 v1) r)
         ) => HRLabelSet (HCons (LVPair l1 v1) (HCons (LVPair l2 v2) r))

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
instance RecordLabels r' ls
      => RecordLabels (HCons (LVPair l v) r') (HCons l ls)

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
      =>   ShowComponents (HCons (LVPair l v) r)
 where
  showComponents comma (HCons f@(LVPair v) r)
     =  comma
     ++ showLabel (labelLVPair f)
     ++ "="
     ++ show v
     ++ showComponents "," r

class ShowLabel l
 where
  showLabel :: l -> String


{-----------------------------------------------------------------------------}

-- Extension for records

instance HRLabelSet (HCons (LVPair l v) r)
    => HExtend (LVPair l v) (Record r) (Record (HCons (LVPair l v) r))
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
 
-- This is a baseline implementation.
-- We use a helper class, HasField, to abstract from the implementation.

class HasField l r v | l r -> v
  where
    hLookupByLabel:: l -> r -> v

{-
instance ( RecordLabels r ls
         , HFind l ls n
         , HLookupByHNat n r (LVPair l v)
         ) => HasField l (Record r) v
  where
    hLookupByLabel l (Record r) = v
      where
        ls = recordLabels r
        n = hFind l ls
        (LVPair v) = hLookupByHNat n r

-}


-- Because hLookupByLabel is so frequent and important, we implement
-- it separately, more efficiently. The algorithm is familiar assq, only
-- the comparison operation is done at compile-time

instance HasField l r v => HasField l (Record r) v where
    hLookupByLabel l (Record r) = hLookupByLabel l r

class HasField' b l r v | b l r -> v where
    hLookupByLabel':: b -> l -> r -> v

instance (HEq l l' b, HasField' b l (HCons (LVPair l' v') r) v)
    => HasField l (HCons (LVPair l' v') r) v where
    hLookupByLabel l r@(HCons f' _) = 
             hLookupByLabel' (hEq l (labelLVPair f')) l r

instance HasField' HTrue l (HCons (LVPair l v) r) v where 
    hLookupByLabel' _ _ (HCons (LVPair v) _) = v
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
  r'   = hUpdateAtHNat n (newLVPair l v) r


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

instance H2ProjectByLabels HNil r HNil r where
    h2projectByLabels _ r = (HNil,r)

instance H2ProjectByLabels (HCons l ls) HNil HNil HNil where
    h2projectByLabels _ _ = (HNil,HNil)

instance (HMemberM l' (HCons l ls) b, 
	  H2ProjectByLabels' b (HCons l ls) (HCons (LVPair l' v') r') rin rout)
    => H2ProjectByLabels (HCons l ls) (HCons (LVPair l' v') r') rin rout where
    -- h2projectByLabels = h2projectByLabels' (undefined::b)
    -- The latter is solely for the Hugs benefit
    h2projectByLabels ls r@(HCons f' _) =h2projectByLabels' (undefined::b) ls r
      -- where b = hMember (labelLVPair f') ls

class H2ProjectByLabels' b ls r rin rout | b ls r -> rin rout where
    h2projectByLabels' :: b -> ls -> r -> (rin,rout)
    
instance H2ProjectByLabels ls' r' rin rout =>
    H2ProjectByLabels' (HJust ls') ls (HCons f' r') (HCons f' rin) rout where
    h2projectByLabels' _ _ (HCons x r) = (HCons x rin, rout)
	where (rin,rout) = h2projectByLabels (undefined::ls') r

instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HNothing ls (HCons f' r') rin (HCons f' rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
	where (rin,rout) = h2projectByLabels ls r


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteAtLabel l r
  r'' = hExtend (newLVPair l' v) r'


{-----------------------------------------------------------------------------}

-- A variation on update: type-preserving update.

hTPupdateAtLabel l v r = hUpdateAtLabel l v r
 where
   te :: a -> a -> ()
   te _ _ = ()
   _ = te v (hLookupByLabel l r)

{-

-- We could also say:

hTPupdateAtLabel l v r = hUpdateAtLabel l v r `asTypeOf` r

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
         , HLeftUnionBool b r (LVPair l v) r'''
         , HLeftUnion (Record r''') (Record r') r''
         )
           => HLeftUnion (Record r) (Record (HCons (LVPair l v) r')) r''
  where
   hLeftUnion (Record r) (Record (HCons f r')) = r''
    where
     b       = hMember (labelLVPair f) (recordLabels r)
     r'''    = hLeftUnionBool b r f
     r''     = hLeftUnion (Record r''') (Record r')

class  HLeftUnionBool b r f r' | b r f -> r'
 where hLeftUnionBool :: b -> r -> f -> r'

instance HLeftUnionBool HTrue r f r
   where hLeftUnionBool _ r _  = r

instance HLeftUnionBool HFalse r f (HCons f r)
   where hLeftUnionBool _ r f = HCons f r


{-----------------------------------------------------------------------------}
-- Compute the symmetric union of two records r1 and r2 and
-- return the pair of records injected into the union (ru1, ru2).
-- To be more precise, we compute the symmetric union _type_ ru
-- of two record _types_ r1 and r2. The emphasis on types is important.
-- The two records (ru1,ru2) in the result of unionSR have the same
-- type ru, but they are generally different values.
-- Here the simple example: suppose
--   r1 = (Label .=. True)  .*. emptyRecord
--   r2 = (Label .=. False) .*. emptyRecord
-- Then unionSR r1 r2 will return (r1,r2). Both components of the result
-- are different records of the same type.

-- To project from the union ru, use hProjectByLabels.
-- It is possible to project from the union obtaining a record
-- that was not used at all when creating the union.
-- We do assure however that if (unionSR r1 r2) gave (r1u,r2u),
-- then projecting r1u onto the type of r1 gives the _value_ identical
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

-- Field f2 is already in r1, so it will be in the union of r1
-- with the rest of r2.
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


{-----------------------------------------------------------------------------}
-- Record equivalence modulo field order
-- Decide if two records r1 and r2 are identical or differ only in the order
-- of their fields.
-- If the two record types are indeed equivalent, return the witness of
-- their equivalence, (HJust (r1->r2,r2->r1)). If they are not equivalent,
-- return HNothing
-- The function equivR does not examine the values of its arguments:
-- it needs only their types.

class RecordEquiv r1 r2 res | r1 r2 -> res where
    equivR :: Record r1 -> Record r2 -> res

class RecordEquiv' b r1 r2 res | b r1 r2 -> res where
    equivR' :: b -> r1 -> r2 -> res

instance (TypeEq r1 r2 b, RecordEquiv' b r1 r2 res) 
    => RecordEquiv r1 r2 res where
    equivR _ _ = equivR' (undefined::b) (undefined::r1) (undefined::r2)


-- Two records have the same type: the fast path
instance RecordEquiv' HTrue r r 
                      (HJust (Record r->Record r,Record r->Record r)) where
    equivR' _ _ _ = HJust (id,id)

-- Try to project one record onto the other
instance (RecordLabels r1 ls1, 
	  RecordLabels r2 ls2,
	  H2ProjectByLabels ls1 r2 res21in res21out, 
	  H2ProjectByLabels ls2 r1 res12in res12out,
	  RecordEquiv'' (r2->(res21in,res21out)) (r1->(res12in,res12out)) res) 
    => RecordEquiv' HFalse r1 r2 res where
    equivR' _ _ _ = equivR'' 
		    (\(r::r2) -> h2projectByLabels (undefined::ls1) r)
		    (\(r::r1) -> h2projectByLabels (undefined::ls2) r)
			     
class RecordEquiv'' p21 p12 res | p21 p12 -> res where
    equivR'' :: p21 -> p12 -> res

instance RecordEquiv'' (r2->(r1,HNil)) (r1->(r2,HNil)) 
                (HJust (Record r1->Record r2,Record r2->Record r1)) where
    equivR'' p21 p12 = HJust (r12,r21)
	where
	r12 (Record r1) = let (rin,_) = p12 r1 in Record rin
	r21 (Record r2) = let (rin,_) = p21 r2 in Record rin

-- r2 has something that r1 doesn't: r2 is a strict superset of r1
instance RecordEquiv'' (r2->(r2',HCons a b)) (r1->(r1',HNil)) HNothing where
    equivR'' _ _ = HNothing

-- r1 is a strict superset of r2
instance RecordEquiv'' (r2->(r2',HNil)) (r1->(r1',HCons a b)) HNothing where
    equivR'' _ _ = HNothing

-- r1 has some fields not in r2, and vice versa
instance RecordEquiv'' (r2->(r2',HCons a b)) (r1->(r1',HCons c d)) 
    HNothing where
    equivR'' _ _ = HNothing

{-
instance RecordEquiv' HFalse (HCons f1 r1) HNil HNothing where
    equivR' _ _ _ = HNothing

instance RecordEquiv' HFalse HNil (HCons f2 r2) HNothing where
    equivR' _ _ _ = HNothing

instance (HEq f1 f2 b, RecordEquiv2 b  (HCons f1 r1) (HCons f2 r2) res)
    => RecordEquiv' HFalse (HCons f1 r1) (HCons f2 r2) res where
    equivR' _ = equivR2 (undefined::b)

class RecordEquiv2 b r1 r2 res | b r1 r2 -> res where
    equivR2 :: b -> r1 -> r2 -> res


instance (RecordEquiv (Record r1) (Record r2) res',
	  RecordEquiv' b r1 r2 res', HEq r1 r2 b,
	  RecordEquiv21 res' (HCons f r1) (HCons f r2) res)
    => RecordEquiv2 HTrue (HCons f r1) (HCons f r2) res where
    equivR2 _ _ _ = equivR21 (equivR (undefined::Record r1)
			             (undefined::Record r2))
		             (undefined::(HCons f r1))
		             (undefined::(HCons f r2))

class RecordEquiv21 b r1 r2 res | b r1 r2 -> res where
    equivR21 :: b -> r1 -> r2 -> res

instance RecordEquiv21 
             (HJust (Record r1->Record r2,Record r2->Record r1))
             (HCons f r1) (HCons f r2)
             (HJust (Record (HCons f r1)->Record (HCons f r2),
	             Record (HCons f r2)->Record (HCons f r1))) where
    equivR21 (HJust (r12,r21)) _ _ = HJust (r12',r21')
      where
      r12' (Record (HCons f r1)) = 
	  let Record r = r12 (Record r1) in Record (HCons f r)
      r21' (Record (HCons f r2)) = 
	  let Record r = r21 (Record r2) in Record (HCons f r)

instance RecordEquiv21 HNothing (HCons f r1) (HCons f r2) HNothing where
    equivR21 _ _ _ = HNothing

-- We've got (HCons f1 r1) (HCons f2 r2) where f1 is not the same as f2
-- Perhaps f1 occurs in r2 and f2 in r1

instance (RecordEquiv (Record r1) (Record r2) res',
	  RecordEquiv' b r1 r2 res', HEq r1 r2 b,
	  RecordEquiv21 res' (HCons f r1) (HCons f r2) res)
    => RecordEquiv2 HFalse (HCons f1 r1) (HCons f2 r2) res where
    equivR2 _ _ _ = equivR21 (equivR (undefined::Record r1)
			             (undefined::Record r2))
		             (undefined::(HCons f r1))
		             (undefined::(HCons f r2))
-}
