{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A model of labels as needed for extensible records.

   Record labels are triplets of type-level naturals, namespace and string.
   The namespace part helps with avoiding confusions between labels from
   different Haskell modules. The string part is simple for an improved
   instance of ShowLabel. This model requires all labels in a record to
   inhabit the same namespace.

-}

 
module Label2 where

import FakePrelude
import HListPrelude
import Record


-- Labels are type-level naturals

data HNat x => Label x ns = Label x ns String deriving Show


-- Construct the first label

firstLabel = Label hZero


-- Construct the next label

nextLabel (Label x ns _) = Label (hSucc x) ns


-- Equality on labels

instance HEq x x' b
      => HEq (Label x ns) (Label x' ns) b


-- Show label

instance HNat x => ShowLabel (Label x ns)
 where
  showLabel (Label _ _ s) = s
