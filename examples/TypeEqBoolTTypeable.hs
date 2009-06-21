{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Implementations of type equality and disequality based on TTypeable.
   This approach works for GHC and Hugs.
-}

module TypeEqBoolTTypeable where

import Data.HList.FakePrelude
-- import Data.HList.TTypeable
-- import Data.HList.TypeEqTTypeable

instance TypeEq x y HTrue  => TypeEqTrue x y
instance TypeEq x y HFalse => TypeEqFalse x y
