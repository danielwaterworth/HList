{-# LANGUAGE CPP #-}
-- | Description: sorting
module Data.HList.HSort where

import Data.HList.HList
import Data.HList.FakePrelude
import Data.HList.Label3

#if __GLASGOW_HASKELL__ > 707
import GHC.TypeLits (type (<=?), CmpSymbol)
-- | only in ghc >= 7.7
instance ((x <=? y) ~ b) => HEqBy HLeFn x y b
-- | only in ghc >= 7.7
instance (HEq (CmpSymbol x y) GT b) => HEqBy HLeFn y x b
#endif

-- | the \"standard\" '<=' for types. Reuses 'HEqBy'
--
-- Note that ghc-7.6 is missing instances for Symbol and Nat, so that
-- sorting only works 'HNat' (as used by "Data.HList.Label3").
data HLeFn

instance HEqByFn HLeFn

instance (HLe x y ~ b) => HEqBy HLeFn x y b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Tagged x v) (Tagged y w) b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Label x) (Label y) b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Proxy x) (Proxy y) b

-- | Data.HList.Label3 labels can only be compared if they belong
-- to the same namespace.
instance (HEqBy HLeFn n m b, ns ~ ns')
     => HEqBy HLeFn (Lbl n ns desc) (Lbl m ns' desc') b


-- | analogous to 'Data.Ord.Down'
data HDown a
instance HEqByFn a => HEqByFn (HDown a)
instance HEqBy f y x b => HEqBy (HDown f) x y b

-- | The HEqBy instances for @HNeq HLeFn@ gives '<'
data HNeq le
instance HEqByFn a => HEqByFn (HNeq a)
instance (HEqBy le x y b1,
          HEqBy le y x b2,
          HAnd b1 (HNot b2) ~ b3)
   => HEqBy (HNeq le) x y b3

{- | @HAlreadySorted le xs b@ is analogous to

> b = all (\(x,y) -> x `le` y) (xs `zip` tail xs)

This optimization is done because GHC-7.8.2's typechecker can sort @[0 .. 100]@
in about 8 seconds with the merge sort as implemented here, while checking that
the list is already sorted takes 3 s.

-}
class HEqByFn le => HAlreadySorted le (xs :: [*]) (b :: Bool) | le xs -> b

instance HEqByFn le => HAlreadySorted le '[x] True
instance HEqByFn le => HAlreadySorted le '[] True
instance (HEqBy le x y b1,
         HAlreadySorted le (x ': ys) b2,
         HAnd b1 b2 ~ b3)  => HAlreadySorted le (x ': y ': ys) b3


-- | merge sort with a special case for sorted lists
class (SameLength a b, HEqByFn le) => HSortBy le (a :: [*]) (b :: [*]) | le a -> b where
    hSortBy :: Proxy le -> HList a -> HList b

hSort xs = hSortBy (Proxy :: Proxy HLeFn) xs

instance (SameLength a b,
          HAlreadySorted le a ok,
          HSortBy1 ok le a b) => HSortBy le a b where
    hSortBy = hSortBy1 (Proxy :: Proxy ok)

instance HSortBy1 True le a a where
    hSortBy1 _ _ a = a -- already sorted

instance HMSortBy le a b => HSortBy1 False le a b where
    hSortBy1 _ = hMSortBy

class HSortBy1 ok le (a :: [*]) (b :: [*]) | ok le a -> b where
    hSortBy1 :: Proxy ok -> Proxy le -> HList a -> HList b

{- | HMSortBy is roughly a transcription of this merge sort

> msort [] = []
> msort [x] = [x]
> msort [x,y] = hSort2 x y
> msort xs = case splitAt (length xs `div` 2) xs of
>              (a,b) -> msort a `merge` msort b

> hSort2 x y
>     | x <= y    = [x,y]
>     | otherwise = [y,x]

> merge (x : xs) (y : ys)
>   | x > y     = y : merge (x : xs) ys
>   | otherwise = x : merge xs (y : ys)

-}
class HMSortBy le (a :: [*]) (b :: [*]) | le a -> b where
    hMSortBy :: Proxy le -> HList a -> HList b


instance HMSortBy le '[] '[] where hMSortBy _ x = x
instance HMSortBy le '[x] '[x] where hMSortBy _ x = x
instance (HSort2 b x y ab, HEqBy le x y b) =>
    HMSortBy le '[x,y] ab where
      hMSortBy _ (a `HCons` b `HCons` HNil) = hSort2 (Proxy :: Proxy b) a b
      hMSortBy _ _ = error "Data.HList.HList.HSortBy impossible"

class HSort2 b x y ab | b x y -> ab where
    hSort2 :: Proxy b -> x -> y -> HList ab

instance HSort2 True x y '[x,y] where
    hSort2 _ x y = x `HCons` y `HCons` HNil

instance HSort2 False x y '[y,x] where
    hSort2 _ x y = y `HCons` x `HCons` HNil

instance (HMerge le xs' ys' sorted,
          HMSortBy le ys ys',
          HMSortBy le xs xs',
          HDiv2 (HLength (a ': b ': c ': cs)) ~ n,
          HSplitAt n (a ': b ': c ': cs) xs ys)
  => HMSortBy le (a ': b ': c ': cs) sorted where
  hMSortBy le abbs = case hSplitAt (Proxy :: Proxy n) abbs of
      (xs, ys) -> hMerge le (hMSortBy le xs) (hMSortBy le ys)


class HMerge le x y xy | le x y -> xy where
    hMerge :: Proxy le -> HList x -> HList y -> HList xy

instance HMerge le '[] '[] '[] where hMerge _ _ _ = HNil
instance HMerge le (x ': xs) '[] (x ': xs) where hMerge _ x _ = x
instance HMerge le '[] (x ': xs) (x ': xs) where hMerge _ _ x = x

instance (HEqBy le x y b,      
          HMerge1 b x y low high,
          HMerge le (high ': xs) ys srt)
    => HMerge le (x ': xs) (y ': ys) (low ': srt) where
  hMerge le (HCons x xs) (HCons y ys) = case hMerge1 (Proxy :: Proxy b) x y of
        (low, high) -> low `HCons` hMerge le (HCons high xs) ys

class HMerge1 b x y min max | b x y -> min max where
    hMerge1 :: Proxy b -> x -> y -> (min, max)

instance HMerge1 True x y x y where
    hMerge1 _ x y = (x,y) 

instance HMerge1 False x y y x where
    hMerge1 _ x y = (y,x) 

-- * More efficient HRLabelSet / HLabelSet
{- | Provided the labels involved have an appropriate instance of HEqByFn,
it would be possible to use the following definitions:

> type HRLabelSet = HSet HLeFn
> type HLabelSet  = HSet HLeFn

-}
class HEqByFn le => HSet le (ps :: [*])
instance (HSortBy le ps ps', HAscList le ps') => HSet le ps

{- |

>>> let xx = Proxy :: HIsSet HLeFn [Label "x", Label "x"] b => Proxy b
>>> :t xx
xx :: Proxy False

>>> let xx = Proxy :: HIsSet HLeFn [Label "x", Label "y"] b => Proxy b
>>> :t xy
xy :: Proxy True

-}
class HEqByFn le => HIsSet le (ps :: [*]) (b :: Bool) | le ps -> b
instance (HSortBy le ps ps', HAlreadySorted le ps' b) => HIsSet le ps b


-- | @HAscList le xs@ confirms that xs is in ascending order,
-- and reports which element is duplicated otherwise.
class HEqByFn le => HAscList le (ps :: [*])

class HAscList1 le (b :: Bool) (ps :: [*])
instance (HAscList1 le (HOr (HNot b1) (HAnd b1 b2)) (y ': ys),
          HEqBy le x y b1,
          HEqBy le y x b2)
  => HAscList le (x ': y ': ys)
instance HEqByFn le => HAscList le '[]
instance HEqByFn le => HAscList le '[x]

instance ( Fail '("Duplicated element", y, "using le", le) )
    => HAscList1 le True (y ': ys)
instance HAscList le ys => HAscList1 le False ys
