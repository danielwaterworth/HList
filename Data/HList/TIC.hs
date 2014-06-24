
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

   (There are other ways: see ConsUnion.hs, for example)
-}

module Data.HList.TIC where

import Data.HList.TIP
import Data.HList.FakePrelude
import Data.HList.HListPrelude

import Data.HList.Record
import Data.HList.Variant
import Data.HList.HList

import Text.ParserCombinators.ReadP
import LensDefs

-- --------------------------------------------------------------------------
-- | A datatype for type-indexed co-products. A 'TIC' is just a 'Variant',
-- where the elements of the type-level list @\"l\"@ are in the form
-- @Tagged x x@.

newtype TIC (l :: [*]) = TIC (Variant l)


-- | @Iso (TIC s) (TIC t) (Variant s) (Variant t)@
--
-- 'typeIndexed' may be more appropriate
ticVariant x = isoNewtype (\(TIC a) -> a) TIC x

-- | @Iso' (TIC s) (Variant s)@
ticVariant' x = simple (ticVariant x)


-- --------------------------------------------------------------------------

{- | Conversion between type indexed collections ('TIC' and 'TIP')
and the corresponding collection that has other label types ('Variant'
and 'Record' respectively)

-}
class TypeIndexed r tr | r -> tr, tr -> r where
    typeIndexed :: forall p f s t a b.
       (TypeIndexedCxt s t a b, Profunctor p, Functor f) =>
      p (tr (TagR a)) (f (tr (TagR b))) -> p (r s) (f (r t))

type TypeIndexedCxt s t a b =
 (HMapCxt HList TaggedFn b t,
  RecordValues s,
  LabelsOf s ~ LabelsOf t,
  a ~ RecordValuesR s,
  b ~ RecordValuesR t,
  {- to use castVariant instead of unsafeCastVariant
  RecordValuesR (TagR a) ~ a,
  RecordValuesR (TagR b) ~ b,
  SameLength (TagR a) s,
  SameLength (TagR b) t,
  -}
  TagUntag a,
  TagUntag b)

instance TypeIndexed Record TIP where
    typeIndexed = unlabeled . fromTipHList
      where fromTipHList = iso (TIP . hTagSelf) (\(TIP a) -> hUntagSelf a)

instance TypeIndexed Variant TIC where
    typeIndexed = isoNewtype unsafeCastVariant unsafeCastVariant
                . isoNewtype TIC (\(TIC a) -> a)

{- |

@'Iso'' ('Variant' s) ('TIC' a)@

@'Iso'' ('Record' s) ('TIP' a)@

where @s@ has a type like @'[Tagged \"x\" Int]@, and
@a@ has a type like @'[Tagged Int Int]@.
-}
typeIndexed' x = simple (typeIndexed x)

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

-- | make a TIC that contains one element
mkTIC1 :: forall i. MkVariant i i '[Tagged i i] => i -> TIC '[Tagged i i]
mkTIC1 i = TIC (mkVariant1 (Label :: Label i) i)

-- | make a TIC for use in contexts where the result type is fixed
mkTIC i = mkTIC' i Proxy


-- --------------------------------------------------------------------------
-- | Public destructor (or, open union's projection function)
instance HasField o (Variant l) (Maybe o) =>
      HasField o (TIC l) (Maybe o) where
    hLookupByLabel l (TIC i) = hLookupByLabel l i


instance (HasField o (TIC l) mo) => HOccurs mo (TIC l) where
    hOccurs = hLookupByLabel (Label :: Label o)

-- | @Prism (TIC s) (TIC t) a b@
ticPrism :: forall p f s t a b. (HPrism a p f s t a b)
  => (a `p` f b) -> (TIC s `p` f (TIC t))
ticPrism = ticVariant . hPrism (Label :: Label a)


-- | @Prism' (TIC s) a@
ticPrism' :: forall p f s t a b. (HPrism a p f s t a b, a~b, s~t)
  => (a `p` f b) -> (TIC s `p` f (TIC t))
ticPrism' = ticVariant . hPrism (Label :: Label a)


-- --------------------------------------------------------------------------
-- | TICs are not opaque

instance ShowVariant l => Show (TIC l)
 where
  showsPrec _ (TIC v) = ("TIC{"++) . showVariant v . ('}':)


instance (ReadVariant l, HAllTaggedEq l, HRLabelSet l) => Read (TIC l)
 where
   readsPrec _ = readP_to_S $ do
     _ <- string "TIC{"
     r <- readVariant
     _ <- string "}"
     return (TIC r)


{- |
> Nothing .*. x = x
> Just a .*. y = mkTIC a
-}
instance (me ~ Maybe e) => HExtend me (TIC l) where
    type HExtendR me (TIC l) = TIC (Tagged (UnMaybe me) (UnMaybe me) ': l)
    Just e .*. _ = TIC (unsafeMkVariant 0 e)
    Nothing .*. TIC x = TIC (extendVariant x)
