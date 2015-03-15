{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Properties.LengthDependentSplice where
import Properties.LengthDependent
import Language.Haskell.TH
import Test.Hspec

hl1_2_3 :: Spec
hl1_2_3 = $(doE $
        [ noBindS
               [| describe $(stringE (show n)) $(hl1 n) |]
            | n <- [1 .. 5]]
    ++ [ noBindS [| describe $(stringE (show (n1,n2))) $(hl2 n1 n2) |]
      | n1 <- [1 .. 3],
        n2 <- [1 .. 3] ]
    ++ [ noBindS [| describe $(stringE (show (n1,n2,n3))) $(hl3 n1 n2 n3) |]
      | n1 <- [0 .. 2],
        n2 <- [0 .. 1],
        n3 <- [0 .. 2],
        not $ all (==0) [n1,n2,n3] ]
  )
