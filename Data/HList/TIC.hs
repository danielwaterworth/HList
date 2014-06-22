
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

   (There are other ways: see ConsUnion.hs, for example)
-}

module Data.HList.TIC where

import Data.Dynamic

import Data.HList.TIP
import Data.HList.FakePrelude

import Data.HList.Record
import Data.HList.Variant

import Text.ParserCombinators.ReadP
import LensDefs

-- --------------------------------------------------------------------------
-- | A datatype for type-indexed co-products

newtype TIC (l :: [*]) = TIC (Variant l)


-- | Iso (TIC s) (TIC t) (Variant s) (Variant t)
ticVariant x = isoNewtype (\(TIC a) -> a) TIC x


-- --------------------------------------------------------------------------
-- | Public constructor (or, open union's injection function)

mkTIC' :: forall i l proxy.
         ( HTypeIndexed l
         , MkVariant i i l
         )
      => i
      -> proxy l -- ^ the ordering of types in the @l :: [*]@ matters.
                 -- This argument is intended to fix the ordering 
                 -- it can be a Record, Variant, TIP, Proxy
      -> TIC l

mkTIC' i p = TIC (mkVariant (Label :: Label i) i p)

mkTIC i = mkTIC' i Proxy


-- --------------------------------------------------------------------------
-- | Public destructor (or, open union's projection function)
instance HasField o (Variant l) (Maybe o) =>
      HasField o (TIC l) (Maybe o) where
    hLookupByLabel l (TIC i) = hLookupByLabel l i

unTIC x = let r = hLookupByLabel (toLabel r) x
              toLabel :: Maybe a -> Label a
              toLabel _ = Label
  in r

-- | @Prism (TIC s) (TIC t) a b@
ticPrism :: forall p f s t a b. (HPrism a p f s t a b)
  => (a `p` f b) -> (TIC s `p` f (TIC t))
ticPrism = ticVariant . hPrism (Label :: Label a)


-- | @Prism' (TIC s) a@
ticPrism' :: forall p f s t a b. (HPrism a p f s t a b, a~b, s~t)
  => (a `p` f b) -> (TIC s `p` f (TIC t))
ticPrism' = ticVariant . hPrism (Label :: Label a)


-- --------------------------------------------------------------------------
-- | TICs are opaque

instance Show (TIC l)
 where
  show _ = "<Cannot show TIC content!>"

