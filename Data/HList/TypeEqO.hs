{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  FlexibleContexts, OverlappingInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic type equality predicate: 
   The implementation based on overlapping instances
   The only place where overlapping instances are really used

-}

module Data.HList.TypeEqO where

import Data.HList.FakePrelude

instance HEq x x True
instance False ~ b => HEq x y b
-- instance TypeEq x y HFalse -- would violate functional dependency


class TupleType t (b :: Bool) | t -> b
instance TupleType () True
instance TupleType (x,y) True
instance TupleType (x,y,z) True
-- Continue for a while
instance False ~ b => TupleType x b
-- instance TupleType x HFalse -- would violate functional dependency
