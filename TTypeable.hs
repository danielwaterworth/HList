{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Typeable at the type level.

-}


module TTypeable where

import Datatypes1
import FakePrelude
import HListPrelude


class TTypeable a b | a-> b

type Zero     = HZero
type One      = HSucc Zero
type Two      = HSucc One
type Three    = HSucc Two
type Four     = HSucc Three
type Five     = HSucc Four
type Six      = HSucc Five
type Seven    = HSucc Six
type Eight    = HSucc Seven
type Nine     = HSucc Eight
type Ten      = HSucc Nine
type Eleven   = HSucc Ten
type Twelf    = HSucc Eleven
type Thirteen = HSucc Twelf


-- Built-in datatypes
instance TTypeable Int     (HCons Zero  HNil)
instance TTypeable Integer (HCons One   HNil)
instance TTypeable Char    (HCons Two   HNil)
instance TTypeable Bool    (HCons Three HNil)


-- Some type constructors
instance (TTypeable a al, TTypeable b bl)
      =>  TTypeable (a->b) (HCons Four (HCons al (HCons bl HNil)))
instance (TTypeable a al)
      =>  TTypeable [a] (HCons Five (HCons al HNil))
instance (TTypeable a al)
      =>  TTypeable (Maybe a) (HCons Six (HCons al HNil))


-- Example of a higher-kind type

data Fix f = Fix (f (Fix f))

instance (TTypeable (f Bool) (HCons al l'))
      =>  TTypeable (Fix f) (HCons Seven (HCons al HNil))


-- User-defined datatypes
instance TTypeable Float   (HCons Eight    HNil)
instance TTypeable Key     (HCons Nine     HNil)
instance TTypeable Name    (HCons Ten      HNil)
instance TTypeable Breed   (HCons Eleven   HNil)
instance TTypeable Price   (HCons Twelf    HNil)
instance TTypeable Disease (HCons Thirteen HNil)
