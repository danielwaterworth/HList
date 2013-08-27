{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Declarations for various classes and functions that
   apply for the whole range of heterogeneous collections
   (HList, TIP, records, etc).
 -}

module Data.HList.HListPrelude where

import Data.HList.FakePrelude

class HExtend e l where
  type HExtendR e l
  (.*.) :: e -> l -> HExtendR e l
infixr 2 .*.

-- Poly-kinded
class SubType l l'

-- subType :: SubType l l' => l -> l' -> ()
-- subType _ _ = ()

-- poly-kinded
class HAppend l1 l2 where
  type HAppendR l1 l2
  hAppend :: l1 -> l2 -> HAppendR l1 l2

-- class HMember e1 l (b :: Bool) | e1 l -> b

-- One occurrence and nothing is left
class HOccurs e l where
  hOccurs :: l -> e

-- Class to test that a type is "free" in a type sequence
-- polykinded
class HOccursNot e l

class HProject l l' where
  hProject :: l -> l'

-- | Map a type (key) to a natural (index) within the collection
-- This is a purely type-level computation
class HType2HNat e l (n :: HNat) | e l -> n

--  | and lift to the list of types
class HTypes2HNats es l (ns :: [HNat]) | es l -> ns

-- | Delete all elements with the type-level key e from the
-- collection l. Since the key is type-level, it is represented
-- by a Proxy.
-- (polykinded)

class HDeleteMany e l l' | e l -> l' where
  hDeleteMany :: Proxy e -> l -> l'

