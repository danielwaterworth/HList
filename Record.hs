{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records

   The are different refinements for this module:
    - LabelHNat.hs --- record labels are simply type-level naturals
    - Label???
    - Label???

-}

 
module Record where

import FakePrelude
import HListPrelude
import HListGoodies
import HArray
import HZip


{-----------------------------------------------------------------------------}

-- Record types as label-value pairs.

newtype Record r = Record r


-- Build a record

mkRecord :: (HZip ls vs r, HLabelSet ls) => r -> Record r
mkRecord = Record


-- Build an empty record

emptyRecord = mkRecord HNil


-- Propery of a proper label set for a record
 
class HLabelSet ls
instance HLabelSet HNil
-- HCons instance is specific to model.


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

instance ( HZip ls vs r
         , HExtend l ls ls'
         , HExtend v vs vs'
         , HLabelSet ls'
         , HZip ls' vs' r'
        )
           => HExtend (l,v) (Record r) (Record r')
 where
  hExtend (l,v) (Record r) = mkRecord r'
   where
    (ls,vs) = hUnzip r
    ls'     = hExtend l ls
    vs'     = hExtend v vs
    r'      = hZip ls' vs'


{-----------------------------------------------------------------------------}

-- Record concatenation

instance ( HAppend r r' r''
         , HZip ls vs r''
         , HLabelSet ls
         )
           => HAppend (Record r) (Record r') (Record r'')
 where
  hAppend (Record r) (Record r')
   =
     mkRecord (hAppend r r')


{-----------------------------------------------------------------------------}

-- Lookup operation

hLookupByLabel (Record r) l = v
 where
   (ls,vs) = hUnzip r
   n       = hFind l ls
   v       = hLookupByHNat vs n


{-----------------------------------------------------------------------------}

-- Delete operation

hDeleteByLabel (Record r) l = Record r'
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls 
  ls'     = hDeleteByHNat ls n
  vs'     = hDeleteByHNat vs n
  r'      = hZip ls' vs'


{-----------------------------------------------------------------------------}

-- Update operation

hUpdateByLabel (Record r) l v = Record (hZip ls vs')
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls
  vs'     = hUpdateByHNat vs n v


{-----------------------------------------------------------------------------}


-- Projection for records

hProjectByLabels (Record r) ls
 = 
   mkRecord (hProjectByLabels' r ls)

class HProjectByLabels r ls r' | r ls -> r'
 where
  hProjectByLabels' :: r -> ls -> r'

instance HProjectByLabels r HNil HNil
 where 
  hProjectByLabels' _ _ = HNil

instance ( HProjectByLabels r ls r''
         , HZip ls' vs r
         , HFind l ls' n
         , HLookupByHNat vs n v
         , HExtend (l,v) r'' r'
         )
      =>   HProjectByLabels r (HCons l ls) r'
 where
  hProjectByLabels' r (HCons l ls) = r'
   where
    r''      = hProjectByLabels' r ls
    (ls',vs) = hUnzip r
    n        = hFind l ls'
    v        = hLookupByHNat vs n
    r'       = hExtend (l,v) r''


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel r l
  r'  = hDeleteByLabel r l
  r'' = hExtend (l',v) r'


{-----------------------------------------------------------------------------}

-- Subtyping for records

instance ( HZip ls vs r'
         , HProjectByLabels (Record r) ls (Record r')
         )
           => SubType (Record r) (Record r')


{-----------------------------------------------------------------------------}
