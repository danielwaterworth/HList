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

hLookupByLabel l (Record r) = v
 where
   (ls,vs) = hUnzip r
   n       = hFind l ls
   v       = hLookupByHNat n vs


{-----------------------------------------------------------------------------}

-- Delete operation

hDeleteByLabel l (Record r) = Record r'
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls 
  ls'     = hDeleteByHNat n ls
  vs'     = hDeleteByHNat n vs
  r'      = hZip ls' vs'


{-----------------------------------------------------------------------------}

-- Update operation

hUpdateByLabel l v (Record r) = Record (hZip ls vs')
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls
  vs'     = hUpdateByHNat n v vs


{-----------------------------------------------------------------------------}


-- Projection for records

hProjectByLabels ls (Record r)
 = 
   mkRecord (hProjectByLabels' ls r)

class HProjectByLabels ls r r' | ls r -> r'
 where
  hProjectByLabels' :: ls -> r -> r'

instance HProjectByLabels HNil r HNil
 where 
  hProjectByLabels' _ _ = HNil

instance ( HProjectByLabels ls r r''
         , HZip ls' vs r
         , HFind l ls' n
         , HLookupByHNat n vs v
         , HExtend (l,v) r'' r'
         )
      =>   HProjectByLabels (HCons l ls) r r'
 where
  hProjectByLabels' (HCons l ls) r = r'
   where
    r''      = hProjectByLabels' ls r
    (ls',vs) = hUnzip r
    n        = hFind l ls'
    v        = hLookupByHNat n vs
    r'       = hExtend (l,v) r''


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteByLabel l r
  r'' = hExtend (l',v) r'


{-----------------------------------------------------------------------------}

-- Subtyping for records

instance ( HZip ls vs r'
         , HProjectByLabels ls (Record r) (Record r')
         )
           => SubType (Record r) (Record r')


{-----------------------------------------------------------------------------}
