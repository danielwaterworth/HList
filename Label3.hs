{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A model of extensible records.

   Record labels are triplets of type-level naturals, namespace and string.
   The namespace part helps with avoiding confusions between labels from
   different Haskell modules. The string part is simple for an improved
   instance of ShowLabel. This model even allows the labels in a record
   to belong to different namespaces. To this end, the model relies on
   the infamouse predicate for type equality.

-}

 
module Label3 (
  module Label3,
  module Record
) where

import FakePrelude
import HListPrelude
import Record


-- Labels are type-level naturals

data Label x ns = Label x ns String deriving Show


-- Public constructors for labels

label :: HNat x => x -> ns -> String -> Label x ns
label =  Label


-- Construct the first label

firstLabel = label HZero


-- Construct the next label

nextLabel (Label x ns _) = label (HSucc x) ns


-- Equality on labels

instance ( HEq x x' b
         , TypeEqBool ns ns' b'
         , HAnd b b' b''
         )
      =>   HEq (Label x ns) (Label x' ns') b''
 where
  hEq (Label x ns _) (Label x' ns' _)
   =
     hAnd (hEq x x') (typeEqBool ns ns')


-- Propery of a proper label set for a record

instance ( HNat x
         , HMember (Label x ns) ls HFalse
         , HLabelSet ls
         )
      =>   HLabelSet (HCons (Label x ns) ls)


-- Show label

instance Show x => ShowLabel (Label x ns)
 where
  showLabel (Label _ _ s) = s
