{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic type equality predicate: 
   The implementation based on overlapping instances
   The only place where overlapping instances are really used

-}

module Data.HList.TypeEqO where

import Data.HList.FakePrelude

#if !NEW_TYPE_EQ
instance HEq x x True
instance False ~ b => HEq x y b
-- instance TypeEq x y HFalse -- would violate functional dependency
#endif



class TupleType t (b :: Bool) | t -> b
instance TupleType () True
instance TupleType (x,y) True
instance TupleType (x,y,z) True
-- Continue for a while
instance False ~ b => TupleType x b
-- instance TupleType x HFalse -- would violate functional dependency

-- overlaps an instance Show (Proxy t) for convenience
instance Show (Proxy True)  where show _ = "HTrue"
instance Show (Proxy False) where show _ = "HFalse"

instance HNat2Integral n => HNat2Integral (HSucc n) where
    hNat2Integral n = hNat2Integral (hPred n) + 1

instance HNat2Integral n => Show (Proxy (n :: HNat)) where
    show n = "H" ++ show (hNat2Integral n :: Integer)


-- | the number of arguments a function can take
class Arity f (n :: HNat) | f -> n
instance hZero ~ HZero => Arity f hZero
instance Arity f n => Arity (x -> f) (HSucc n)
