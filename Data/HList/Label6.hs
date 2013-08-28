{-# LANGUAGE KindSignatures, DataKinds, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Yet another model of labels.

   Labels are promoted Strings or Integers "GHC.TypeLits" inside the
   'Label'.

   See "Data.HList.MakeLabels" for an alternative, slightly more convenient method.
-}

module Data.HList.Label6 where

import Data.HList.Record
import GHC.TypeLits

instance SingI x => ShowLabel (x :: Symbol) where
  showLabel _ =  fromSing (sing :: Sing x)
