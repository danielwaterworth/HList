{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Yet another model of labels.
   Labels are just marked silly datatypes.

-}

 
module Label4 where

import Data.Typeable
import Data.Char
import FakePrelude
import HListPrelude
import Record
import TIP


-- Equality on labels

instance TypeEq x y b => HEq (Proxy x) (Proxy y) b


-- Propery of a proper label set for a record

instance ( HMember (Proxy x) ls HFalse
         , HLabelSet ls
         )
      =>   HLabelSet (HCons (Proxy x) ls)



-- Show label

instance Typeable x => ShowLabel (Proxy x)
 where
  showLabel = (\(x:xs) -> toLower x:xs)
            . reverse
            . takeWhile (not . (==) '.')
            . reverse
            . tyconString
            . typerepTyCon
            . typeOf
            . unProxy
