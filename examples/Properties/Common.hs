{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Properties.Common where

import Data.HList.CommonMain
import Test.QuickCheck
import Data.Array.Unboxed
import Data.HList.Variant
import Data.Monoid
import Control.Lens
import Control.Applicative
import GHC.TypeLits (Symbol)
import Language.Haskell.TH

hListT :: [TypeQ] -> TypeQ
hListT = foldr (\a b -> [t| $a ': $b |]) promotedNilT

hListE :: [ExpQ] -> ExpQ
hListE = foldr (\a b -> [| $a `HCons` $b |]) [| HNil |]

hNatE :: Int -> ExpQ
hNatE n = foldr appE [| hZero |] (replicate n [| hSucc |])

hNatT :: Int -> TypeQ
hNatT n = foldr appT [t| HZero |] (replicate n [t| HSucc |])


lx = Label :: Label "x"
ly = Label :: Label "y"
lz = Label :: Label "z"

data BinF b = BinF (b -> b -> b)

instance (bb ~ (b, b), b ~ b') => ApplyAB (BinF b') bb b where
    applyAB (BinF f) = uncurry f


-- | A more general type than @===@ used to
-- ensure that both sides can infer the same type
eq :: (Show a, Show b, HCast a b, HCast b a, Eq a, Eq b) => a -> b -> Property
eq x y = hCast x === Just y .&&. Just x === hCast y
infix 4 `eq`

data HSuccF = HSuccF

instance (psn ~ Proxy (HSucc n),
        pn ~ Proxy n) => ApplyAB HSuccF pn psn where
    applyAB _ = hSucc


data HSplitAtAppend l = HSplitAtAppend (HList l)
instance (pn ~ Proxy n,
          HSplitAt n l a b,
          HAppend (HList a) (HList b),
          y ~ HAppendR (HList a) (HList b)) => ApplyAB (HSplitAtAppend l) pn y where
    applyAB (HSplitAtAppend l) n = case hSplitAt n l of
                                     (a,b) -> hAppend a b



data ConstTrue
instance HEqByFn ConstTrue
instance HEqBy ConstTrue x y True

data ConstFalse
instance HEqByFn ConstFalse
instance HEqBy ConstFalse x y False


instance Arbitrary a => Arbitrary (Tagged t a) where
    arbitrary = fmap Tagged arbitrary

instance Arbitrary (HList '[]) where
    arbitrary = return HNil

instance (Arbitrary x, Arbitrary (HList xs)) => Arbitrary (HList (x ': xs)) where
    arbitrary = do
      x <- arbitrary
      xs <- arbitrary
      return $ x `HCons` xs

instance (Arbitrary x, Arbitrary (Variant (Tagged t y ': ys)),
          HExtend (Tagged s (Maybe x)) (Variant (Tagged t y ': ys)),
          HNat2Integral (HLength ys))
        => Arbitrary (Variant (Tagged s x ': Tagged t y ': ys)) where
    arbitrary = do
      let nys = hNat2Integral (Proxy :: Proxy (HLength ys))
      x :: Maybe x <- frequency [ (1, Just <$> arbitrary), (nys+1, return Nothing) ]
      yys :: Variant (Tagged t y ': ys) <- arbitrary
      return $ Tagged x .*. yys

instance Arbitrary z => Arbitrary (Variant '[Tagged t z]) where
    arbitrary = do
      z <- arbitrary
      return $ mkVariant1 Label z

instance (CoArbitrary x, CoArbitrary (Variant (y ': z)),
         HNat2Integral n, n ~ HLength (y ': z)) => CoArbitrary (Variant (x ': y ': z)) where
    coarbitrary v = case splitVariant1' v of
          Left x -> variant (hNat2Integral (Proxy :: Proxy n) :: Int) . coarbitrary x
          Right v' -> coarbitrary v'

instance (CoArbitrary v, Unvariant '[Tagged t v] v) => CoArbitrary (Variant '[Tagged t v]) where
    coarbitrary v = coarbitrary (unvariant v)

-- | This type is used to make unique types with two members.
--
-- > (BoolN True :: BoolN "x") /= (BoolN True :: BoolN "y")
--
-- is a type error
newtype BoolN (n :: Symbol) = BoolN Bool
  deriving (Eq,CoArbitrary,Arbitrary,Show,Read,Ord)

boolN next = simple $ iso (\(BoolN x) -> x) BoolN next

instance Monoid (BoolN n) where
    mempty = BoolN (getAll mempty)
    mappend (BoolN x) (BoolN y) = BoolN (getAll (mappend (All x) (All y)))

instance Arbitrary (Identity (BoolN n)) where
    arbitrary = fmap return arbitrary


data HSortF = HSortF
instance (x ~ Record xs,
          y ~ Record ys,
          HRLabelSet ys,
          HSort xs ys) => ApplyAB HSortF x y where
  applyAB _ (Record x) = mkRecord (hSort x)
