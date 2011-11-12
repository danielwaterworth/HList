{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  FlexibleContexts, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic type equality predicate: 
   The implementation based on overlapping instances

-}

module Data.HList.TypeEqO where

import Data.HList.FakePrelude

instance TypeEq x x HTrue
instance (HBool b, HFalse ~ b) => TypeEq x y b
-- instance TypeEq x y HFalse -- would violate functional dependency


class HBool b => TupleType t b | t -> b
instance TupleType () HTrue
instance TupleType (x,y) HTrue
instance TupleType (x,y,z) HTrue
-- Continue for a while
instance (HBool b, HFalse ~ b) => TupleType x b
-- instance TupleType x HFalse -- would violate functional dependency
