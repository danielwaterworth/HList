{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Typeable at the type level.

-}


module TTypeable where

import Datatypes
import FakePrelude
import HList


class TTypeable a b | a-> b

type Zero   = HZero
type One    = HSucc Zero
type Two    = HSucc One
type Three  = HSucc Two
type Four   = HSucc Three
type Five   = HSucc Four
type Six    = HSucc Five
type Seven  = HSucc Six
type Eight  = HSucc Seven
type Nine   = HSucc Eight
type Ten    = HSucc Nine
type Eleven = HSucc Ten
type Twelf  = HSucc Eleven


-- Built-in datatypes
instance TTypeable Integer (HCons Zero  HNil)
instance TTypeable Char    (HCons One   HNil)
instance TTypeable Bool    (HCons Two   HNil)


-- Some type constructors
instance (TTypeable a al, TTypeable b bl)
      =>  TTypeable (a->b) (HCons Three (HCons al (HCons bl HNil)))
instance (TTypeable a al)
      =>  TTypeable [a] (HCons Four (HCons al HNil))
instance (TTypeable a al)
      =>  TTypeable (Maybe a) (HCons Five (HCons al HNil))


-- Example of a higher-kind type

data Fix f = Fix (f (Fix f))

instance (TTypeable (f Bool) (HCons al l'))
      =>  TTypeable (Fix f) (HCons Six (HCons al HNil))


-- User-defined datatypes
instance TTypeable Float   (HCons Seven  HNil)
instance TTypeable Key     (HCons Eight  HNil)
instance TTypeable Name    (HCons Nine   HNil)
instance TTypeable Breed   (HCons Ten    HNil)
instance TTypeable Price   (HCons Eleven HNil)
instance TTypeable Disease (HCons Twelf  HNil)


-- Equality predicate on type-level type representations
class TTypeableEqBool x y r | x y -> r
instance TTypeableEqBool HNil HNil        HTrue
instance TTypeableEqBool HNil (HCons a b) HFalse
instance TTypeableEqBool (HCons a b) HNil HFalse
instance TTypeableEqBool l l' r =>
         TTypeableEqBool (HCons HZero l) (HCons HZero l') r
instance TTypeableEqBool (HCons HZero l) (HCons (HSucc a) l') HFalse
instance TTypeableEqBool (HCons (HSucc a) l') (HCons HZero l) HFalse
instance TTypeableEqBool (HCons n l) (HCons n' l') r =>
         TTypeableEqBool (HCons (HSucc n) l) (HCons (HSucc n') l') r
instance TTypeableEqBool l l' r =>
         TTypeableEqBool (HCons HNil l) (HCons HNil l') r
instance TTypeableEqBool (HCons HNil l) (HCons (HCons a' t') l') HFalse
instance TTypeableEqBool (HCons (HCons a' t') l') (HCons HNil l) HFalse
instance TTypeableEqBool (HCons a (HCons t l)) (HCons a' (HCons t' l')) r =>
         TTypeableEqBool (HCons (HCons a t) l) (HCons (HCons a' t') l') r


-- Generic implementation of the type equality predicate
instance ( TTypeable t tt
         , TTypeable t' tt'
         , TTypeableEqBool tt tt' b
         )
      =>   TypeEqBool t t' b


-- Rely on lazy show for type-level Booleans
teq :: TypeEqBool t t' b => t -> t' -> b
teq = undefined


type Integer3 = Integer->Integer->Integer

test = [
        show$ teq not (&&),
        show$ teq not not,

{-

    Context reduction stack overflow; size = 21
    Use -fcontext-stack20 to increase stack size to (e.g.) 20

-}

--        show$ teq (&&) (||),
--        show$ teq ((+)::Integer3) ((-)::Integer3),
--        show$ teq ((*)::Integer3) ((*)::Integer3),
--        show$ teq ((*)::Integer3) not,
--        show$ teq True False,
        show$ teq (1::Integer) True,
        show$ teq False ((+)::Integer3),
        show$ teq (||) ((+)::Integer3),
        show$ teq (undefined::Fix Maybe) True,
        show$ teq (undefined::Fix Maybe) (undefined::Fix []),
        show$ teq (undefined::Fix Maybe) (undefined::Fix Maybe)
       ]
