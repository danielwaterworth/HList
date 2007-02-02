{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 
   The HList library

   A heterogeneous version of
	sequence :: (Monad m) => [m a] -> m [a]

   Only now we operate on heterogeneous lists, where different elements
   may have different types 'a'. 
   In the argument list of monadic values (m a_i),
   although a_i may differ, the monad 'm' must be the same for all
   elements. That's why we need TypeCast. The typechecker will complain
   if we attempt to use hSequence on a HList of monadic values with different
   monads.

   The hSequence problem was posed by Matthias Fischmann
   in his message on the Haskell-Cafe list on Oct 8, 2006
   http://www.haskell.org/pipermail/haskell-cafe/2006-October/018708.html
   http://www.haskell.org/pipermail/haskell-cafe/2006-October/018784.html
 -}

module HSequence (hSequence) where

import HListPrelude
import Control.Monad (liftM2)

import TypeCastGeneric2			-- For tests


data ConsM

instance (TypeCast (m1 l) (m l), Monad m) 
    => Apply ConsM (m a, m1 l) (m (HCons a l)) where
    apply _ (me,ml) = liftM2 HCons me (typeCast ml)

-- Inferred type:
-- hSequence :: (Monad m, HFoldr ConsM (m HNil) l r) => l -> r
hSequence l = hFoldr (undefined::ConsM) (return HNil) l


-- Tests

hlist = HCons (Just 1) (HCons (Just 'c') HNil) -- Maybe monad
hlist2 = HCons ([1]) (HCons (['c']) HNil)      -- List monad

testHSequence  = hSequence hlist
testHSequence2 = hSequence hlist2

main = do
       print testHSequence
       print testHSequence2