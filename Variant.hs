{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Variants, i.e., labelled sums.

   One approach to their implementation would be to consider both 
   the favoured label and the corresponding value as dynamics upon
   variant construction. Since we are too lazy to programme some
   Typeable instances for non-ghc systems (NB: in GHC, Typeable
   is derivable), we rather model variants as (opaque) records
   with maybies for the values. Only one value will actually hold
   non-Nothing, as guaranteed by the constructor.

-}

 
module Variant where

import FakePrelude
import HListPrelude
import HArray
import HOccurs
import HZip
import Record
import TIC


{-----------------------------------------------------------------------------}

-- Variant types on the basis of label-maybe pairs.

data Variant mr = Variant mr


{-----------------------------------------------------------------------------}

-- Turn proxy sequence into sequence of Nothings

class HMaybied l l' | l -> l', l' -> l
 where
  hMaybied :: l -> l'

instance HMaybied HNil HNil
 where 
  hMaybied _ = HNil

instance HMaybied l l'
      => HMaybied (HCons (Proxy e) l) (HCons (Maybe e) l')
 where
  hMaybied (HCons _ l) = HCons Nothing (hMaybied l)


{-----------------------------------------------------------------------------}

-- Public constructor

mkVariant :: ( HZip ls ps v
             , HLabelSet ls
             , HTypeProxied ps
             , HFind x ls n
             , HLookupByHNat n ps (Proxy y)
             , HMaybied ps ms
             , HUpdateAtHNat n (Maybe y) ms ms
             , HZip ls ms v'
             ) 
          => x -> y -> (Record v) -> Variant v'

mkVariant x y (Record v) = Variant v'
 where
  (ls,ps) = hUnzip v
  n       = hFind x ls
  ms      = hMaybied ps
  ms'     = hUpdateAtHNat n (Just y) ms
  v'      = hZip ls ms'


{-----------------------------------------------------------------------------}

-- Public destructor

unVariant :: ( HZip ls ms v
             , HFind x ls n
             , HLookupByHNat n ms (Maybe y)
             )
          => x -> Variant v -> Maybe y

unVariant x (Variant v) = y
 where
  (ls,ms) = hUnzip v
  n       = hFind x ls
  y       = hLookupByHNat n ms


{-----------------------------------------------------------------------------}

-- Variants are opaque

instance Show (Variant v)
 where
  show _ = "<Cannot show Variant content!>"


{-----------------------------------------------------------------------------}
