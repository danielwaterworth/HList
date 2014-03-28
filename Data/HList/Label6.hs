{-# LANGUAGE CPP #-}
{- |
   Description : labels using promoted strings (Symbol)

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke


   Yet another model of labels.

   Labels are promoted Strings or Integers "GHC.TypeLits" inside the
   'Label'. Needs ghc7.6 or higher.

   See "Data.HList.CommonMain#label6demo" for an example.

-}

module Data.HList.Label6 () where

import Data.HList.FakePrelude
import GHC.TypeLits

#if MIN_VERSION_base(4,7,0)
instance KnownSymbol x => ShowLabel (x :: Symbol) where
  showLabel _ =  symbolVal (Proxy :: Proxy x)
#else
instance SingI x => ShowLabel (x :: Symbol) where
  showLabel _ =  fromSing (sing :: Sing x)
#endif
