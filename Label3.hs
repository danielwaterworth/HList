{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A model of labels as needed for extensible records.

   Record labels are triplets of type-level naturals, namespace and string.
   The namespace part helps with avoiding confusions between labels from
   different Haskell modules. The string part is simple for an improved
   instance of ShowLabel.

   This model even allows the labels in a record to belong to different
   namespaces. To this end, the model employs the predicate for type
   equality.

-}

 
module Label3 where

import FakePrelude
import HListPrelude
import Record


-- Labels are type-level naturals

data Label x ns = Label x ns String

instance ShowLabel (Label x ns) => Show (Label x ns)
 where
  show = showLabel


-- Public constructors for labels

label :: HNat x => x -> ns -> String -> Label x ns
label =  Label


-- Construct the first label

firstLabel = label hZero


-- Construct the next label

nextLabel (Label x ns _) = label (hSucc x) ns


-- Equality on labels

instance ( HEq x x' b
         , TypeEq ns ns' b'
         , HAnd b b' b''
         )
      =>   HEq (Label x ns) (Label x' ns') b''


-- Show label

instance Show x => ShowLabel (Label x ns)
 where
  showLabel (Label _ _ s) = s
