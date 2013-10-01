{-# LANGUAGE CPP #-}

{- | Injection from algebraic kinds to *

Algebraic kinds like Nat are not populated and we can't use 
values of type Nat as function arguments. In contrast, we can use
(undefined :: Proxy Z) as an argument, as a value proxy.
data Proxy (tp :: k) :: *

re-exports 'Data.Typeable.Proxy' if it exists
-}
module Data.HList.FakePrelude.Proxy (Proxy, proxy) where

#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Proxy(..))
#else

data Proxy tp 
#endif

proxy :: Proxy tp
proxy =  undefined

