{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records

   The are different models of labels that go with this module;
   see the files Label?.hs.

-}

 
module Record where

import FakePrelude
import HListPrelude
import HArray
import HZip


{-----------------------------------------------------------------------------}

-- Record types as label-value pairs.

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
	  HRLabelSet (HCons (l2,v2) r),
	  HRLabelSet (HCons (l1,v1) r))
    => HRLabelSet (HCons (l1,v1) (HCons (l2,v2) r))
{-
instance (HZip ls vs ps, HLabelSet ls) => HRLabelSet ps
-}

class HLabelSet ls
instance HLabelSet HNil
instance (HMember x ls HFalse, HLabelSet ls)
      =>  HLabelSet (HCons x ls)


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
      =>   ShowComponents (HCons (l,v) r)
 where
  showComponents comma (HCons (l,v) r)
     =  comma
     ++ showLabel l 
     ++ "="
     ++ show v
     ++ showComponents "," r

class ShowLabel l
 where
  showLabel :: l -> String


{-----------------------------------------------------------------------------}

-- Extension for records

instance HRLabelSet (HCons (l,v) r)
    => HExtend (l,v) (Record r) (Record (HCons (l,v) r))
 where
  hExtend (l,v) (Record r) = mkRecord (HCons (l,v) r)


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

instance (HEq l l' b, HasField' b l (HCons (l',v') r) v)
    => HasField l (HCons (l',v') r) v where
    hLookupByLabel l r@(HCons (l',_) _) = hLookupByLabel' (hEq l l') l r

instance HasField' HTrue l (HCons (l,v) r) v where 
    hLookupByLabel' _ _ (HCons (_,v) _) = v
instance HasField l r v => HasField' HFalse l (HCons (l',v') r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l r



{-----------------------------------------------------------------------------}

-- Delete operation

hDeleteAtLabel l (Record r) = Record r'
 where
  (_,r')  = h2projectByLabels (HCons l HNil) r


{-----------------------------------------------------------------------------}

-- Update operation

hUpdateAtLabel l v (Record r) = Record (hZip ls vs')
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls
  vs'     = hUpdateAtHNat n v vs


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
	  H2ProjectByLabels' b ls (HCons (l',v') r') rin rout)
    => H2ProjectByLabels ls (HCons (l',v') r') rin rout where
    h2projectByLabels ls r@(HCons (l',_) _) =
	h2projectByLabels' (hMember l' ls) ls r

class H2ProjectByLabels' b ls r rin rout | b ls r -> rin rout where
    h2projectByLabels' :: b -> ls -> r -> (rin,rout)
    
instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HTrue ls (HCons (l,v') r')
			     (HCons (l,v') rin) rout where
    h2projectByLabels' _ ls (HCons x r) = (HCons x rin, rout)
	where (rin,rout) = h2projectByLabels ls r

instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HFalse ls (HCons (l',v') r')
		              rin (HCons (l',v') rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
	where (rin,rout) = h2projectByLabels ls r


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteAtLabel l r
  r'' = hExtend (l',v) r'


{-----------------------------------------------------------------------------}

-- A variation on update: type-preserving update.

hTPupdateAtLabel l (v::v) r = hUpdateAtLabel l v r
 where
  (_::v) = hLookupByLabel l r


{-----------------------------------------------------------------------------}

-- Subtyping for records

instance ( HZip ls vs r'
         , H2ProjectByLabels ls r r' rout
         )
    => SubType (Record r) (Record r')


{-----------------------------------------------------------------------------}

class  HLeftUnion r r' r'' | r r' -> r''
 where hLeftUnion :: r -> r' -> r''

instance HLeftUnion r (Record HNil) r
 where   hLeftUnion r _ = r

instance ( HZip ls vs r
         , HMember l ls b
         , HLeftUnionBool b r l v r'''
         , HLeftUnion (Record r''') (Record r') r''
         )
           => HLeftUnion (Record r) (Record (HCons (l,v) r')) r''
  where
   hLeftUnion (Record r) (Record (HCons (l,v) r')) = r''
    where
     (ls,vs) = hUnzip r
     b       = hMember l ls
     r'''    = hLeftUnionBool b r l v
     r''     = hLeftUnion (Record r''') (Record r')

class  HLeftUnionBool b r l v r' | b r l v -> r'
 where hLeftUnionBool :: b -> r -> l -> v -> r'

instance HLeftUnionBool HTrue r l v r
   where hLeftUnionBool _ r _ _ = r

instance HLeftUnionBool HFalse r l v (HCons (l,v) r)
   where hLeftUnionBool _ r l v = HCons (l,v) r


{-----------------------------------------------------------------------------}
