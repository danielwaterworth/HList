{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

   (There are other ways: see ConsUnion.hs, for example)
-}

module Data.HList.TIC where

import Data.Dynamic

import Data.HList.HList
import Data.HList.TIP


-- --------------------------------------------------------------------------
-- | A datatype for type-indexed co-products

newtype TIC (l :: [*]) = TIC Dynamic


-- --------------------------------------------------------------------------
-- | Public constructor (or, open union's injection function)

mkTIC :: ( HTypeIndexed l
         , HMember i l True
         , Typeable i
         )
      => i -> TIC l

mkTIC i = TIC (toDyn i)


-- --------------------------------------------------------------------------
-- | Public destructor (or, open union's projection function)

unTIC :: ( HTypeIndexed l
         , HMember o l True
         , Typeable o
         )
      => TIC l -> Maybe o

unTIC (TIC i) = fromDynamic i


-- --------------------------------------------------------------------------
-- | TICs are opaque

instance Show (TIC l)
 where
  show _ = "<Cannot show TIC content!>"

