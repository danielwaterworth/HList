{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- ghc-7.8 has no Typeable (x :: Symbol), so use OldTypeable
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Description: quickcheck tests
--
-- Many of the tests here use quickcheck. The lengths of the hlists
-- involved exhaustively cover a small range, while the elements
-- are random samples.
module Main where

import Test.Hspec

import Properties.LengthDependentSplice
import Properties.LengthIndependent

main = hspec $ do
   hl0
   hl1_2_3








