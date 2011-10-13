{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
   The HList library

   A heterogeneous version of

   > sequence :: (Monad m) => [m a] -> m [a]

   Only now we operate on heterogeneous lists, where different elements
   may have different types 'a'.
   In the argument list of monadic values (m a_i),
   although a_i may differ, the monad 'm' must be the same for all
   elements. That's why we need "Data.HList.TypeCastGeneric2".
   The typechecker will complain
   if we attempt to use hSequence on a HList of monadic values with different
   monads.

   The 'hSequence' problem was posed by Matthias Fischmann
   in his message on the Haskell-Cafe list on Oct 8, 2006

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018708.html>

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018784.html>
 -}

module Data.HList.HSequence (hSequence) where

import Data.HList.HListPrelude -- (Apply(..), HCons(..), apply, hFoldr, HNil)
import Control.Monad (liftM2)


data ConsM = ConsM

instance (m1 ~ m, Monad m) => Apply ConsM (m a, m1 l) (m (HCons a l)) where
    apply _ (me,ml) = liftM2 HCons me ml

-- Inferred type:
-- hSequence :: (Monad m, HFoldr ConsM (m HNil) l r) => l -> r
hSequence l = hFoldr ConsM (return HNil) l


-- Tests

hlist  = Just (1 :: Integer) `hCons` (Just 'c') `hCons` hNil -- Maybe monad
hlist2 = [1] `hCons` ['c'] `hCons` hNil                      -- List monad

testHSequence  = hSequence hlist
-- Just (HCons 1 (HCons 'c' HNil))

testHSequence2 = hSequence hlist2
-- [HCons 1 (HCons 'c' HNil)]

