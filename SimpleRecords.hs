{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records --- a simple model based on type-level naturals.

-}

 
module SimpleRecords where

import FakePrelude
import HListPrelude
import HListGoodies
import HArray
import HZip


{-----------------------------------------------------------------------------}

--
-- Simple record types as label-value pairs.
-- Labels are plain type-level naturals.
--

newtype SimpleRecord r = SimpleRecord r deriving Show

-- Build a record

mkSimpleRecord :: ( HZip ns vs r
                  , HNats ns
                  , HSet ns
                  )
               => r -> SimpleRecord r
mkSimpleRecord = SimpleRecord


-- Build an empty record

emptySimpleRecord = mkSimpleRecord HNil



{-----------------------------------------------------------------------------}

-- Lifted hAppend

instance ( HAppend r r' r''
         , HZip ns vs r''
         , HNats ns
         , HSet ns
         )
           => HAppend (SimpleRecord r) (SimpleRecord r') (SimpleRecord r'')
 where
  hAppend (SimpleRecord r) (SimpleRecord r')
   =
     mkSimpleRecord (hAppend r r')


{-----------------------------------------------------------------------------}

-- Lookup operation

class HLookup r l v | r l -> v
 where
  hLookup :: r -> l -> v

instance ( HZip ls vs r
         , HFind l ls n
         , HLookupByHNat vs n v
         )
           => HLookup (SimpleRecord r) l v
 where
  hLookup (SimpleRecord r) l = v
   where
    (ls,vs) = hUnzip r
    n       = hFind l ls
    v       = hLookupByHNat vs n


{-----------------------------------------------------------------------------}

-- Lifted hDelete

class HDelete r l r' | r l -> r'
 where
  hDelete :: r -> l -> r'

instance ( HZip ls vs r
         , HFind l ls n
         , HDeleteByHNat ls n ls'
         , HDeleteByHNat vs n vs'
         , HZip ls' vs' r'
         )
           => HDelete (SimpleRecord r) l (SimpleRecord r')
 where
  hDelete (SimpleRecord r) l = SimpleRecord r'
   where
    (ls,vs) = hUnzip r
    n       = hFind l ls 
    ls'     = hDeleteByHNat ls n
    vs'     = hDeleteByHNat vs n
    r'      = hZip ls' vs'


{-----------------------------------------------------------------------------}

-- Lifted hUpdate

class HUpdate r l v
 where
  hUpdate :: r -> l -> v -> r

instance ( HZip ls vs r
         , HFind l ls n
         , HUpdateByHNat vs n v vs
         )
           => HUpdate (SimpleRecord r) l v
 where
  hUpdate (SimpleRecord r) l v = SimpleRecord (hZip ls vs')
   where
    (ls,vs) = hUnzip r
    n       = hFind l ls
    vs'     = hUpdateByHNat vs n v


{-----------------------------------------------------------------------------}

-- Extension for records

instance ( HZip ls vs r
         , HExtend l ls ls'
         , HExtend v vs vs'
         , HNats ls'
         , HSet ls'
         , HZip ls' vs' r'
        )
           => HExtend (l,v) (SimpleRecord r) (SimpleRecord r')
 where
  hExtend (l,v) (SimpleRecord r) = mkSimpleRecord r'
   where
    (ls,vs) = hUnzip r
    ls'     = hExtend l ls
    vs'     = hExtend v vs
    r'      = hZip ls' vs'


{-----------------------------------------------------------------------------}

-- Projection for records

class HProject r ls r' | r ls -> r'
 where
  hProject :: r -> ls -> r'

instance HProject (SimpleRecord l) HNil (SimpleRecord HNil)
 where 
  hProject _ _ = emptySimpleRecord

instance ( HProject r ls r''
         , HLookup r l v
         , HExtend (l,v) r'' r'
         )
      =>   HProject r (HCons l ls) r'
 where
  hProject r (HCons l ls) = r'
   where
    r'' = hProject r ls
    v   = hLookup r l
    r'  = hExtend (l,v) r''


{-----------------------------------------------------------------------------}

-- Rename the label of record
 
hRename l l' r = r''
 where
  v   = hLookup r l
  r'  = hDelete r l
  r'' = hExtend (l',v) r'


{-----------------------------------------------------------------------------}

-- Subtyping for records

instance ( HZip ls vs r'
         , HProject (SimpleRecord r) ls (SimpleRecord r')
         )
           => SubType (SimpleRecord r) (SimpleRecord r')


{-----------------------------------------------------------------------------}
