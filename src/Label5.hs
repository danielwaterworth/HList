{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Yet another model of labels.
   This model allow us to use any type as label type.
   As a result, we need some generic instances.
   Also, type errors may be more confusing now.

-}


module Label5 where

import Data.Typeable
import Data.Char
import FakePrelude
import Record


-- Equality on labels

instance TypeEq x y b => HEq x y b


-- Show label

instance Typeable x => ShowLabel x
 where
  showLabel = (\(x:xs) -> toLower x:xs)
            . reverse
            . takeWhile (not . (==) '.')
            . reverse
	    . show
{-
            . tyConString
            . typeRepTyCon
-}
            . typeOf
