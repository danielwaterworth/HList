{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Basic declarations for typeful heterogeneous lists.

   Excuse the unstructured haddocks: while there are many declarations here
   some are alternative implementations should be grouped, and the definitions
   here are analgous to many list functions in the "Prelude".
 -}

module Data.HList.HList where

import Data.HList.FakePrelude
import Data.HList.HListPrelude

import Control.Applicative (Applicative, liftA2, pure)


-- --------------------------------------------------------------------------
-- * Heterogeneous type sequences
-- $note
--
-- The easiest way to ensure that sequences can only be formed with Nil
-- and Cons is to use GADTs
-- The kind [*] is list kind (lists lifted to types)

data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)


-- ** Alternative representation
{- $note

HNil' and HCons' are the older ADT-style. This has some advantages
over the GADT:

* lazy pattern matches are allowed

* type inference is better if you want to directly pattern match
<http://stackoverflow.com/questions/19077037/is-there-any-deeper-type-theoretic-reason-ghc-cant-infer-this-type see stackoverflow post here>

-}
data HNil' = HNil'
data HCons' a b = HCons' a b


-- | conversion between GADT ('HList') and ADT ('HNil'' 'HCons'')
-- representations
class (UnPrime (Prime a) ~ a) => ConvHList (a :: [*]) where
    type Prime a :: *
    type UnPrime b :: [*]
    prime :: HList a -> Prime a
    unPrime :: Prime a -> HList a

instance ConvHList as => ConvHList (a ': as) where
    type Prime   (a ': as) = a `HCons'` Prime as
    type UnPrime (b `HCons'` bs) = (b ': UnPrime bs)
    prime (a `HCons` as) = a `HCons'` prime as
    unPrime ~(a `HCons'` as) = a `HCons` unPrime as

instance ConvHList '[] where
    type Prime '[] = HNil'
    type UnPrime HNil' = '[]
    prime _ = HNil'
    unPrime _ = HNil




instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (HCons x l) = let 'H':'[':s = show l
		       in "H[" ++ show x ++ 
			          (if s == "]" then s else ", " ++ s)

infixr 2 `HCons`


-- --------------------------------------------------------------------------
-- * Basic list functions

-- | 'head'
hHead :: HList (e ': l) -> e
hHead (HCons x _) = x

-- | 'tail'
hTail :: HList (e ': l) -> HList l
hTail (HCons _ l) = l

-- | Length
type family HLength (x :: [*]) :: HNat
type instance HLength '[] = HZero
type instance HLength (x ': xs) = HSucc (HLength xs)

hLength   :: HList l -> Proxy (HLength l)
hLength _ =  undefined

-- ** Append
instance HExtend e (HList l) where
  type HExtendR e (HList l) = HList (e ': l)
  (.*.) = HCons

instance HAppend (HList l1) (HList l2) where
  type HAppendR (HList l1) (HList l2) = HList (HAppendList l1 l2)
  hAppend = hAppendList

type family HAppendList (l1 :: [*]) (l2 :: [*]) :: [*]
type instance HAppendList '[] l = l
type instance HAppendList (e ': l) l' = e ': HAppendList l l'

-- | the same as 'hAppend'
hAppendList :: HList l1 -> HList l2 -> HList (HAppendList l1 l2)
hAppendList HNil l = l
hAppendList (HCons x l) l' = HCons x (hAppend l l')

-- --------------------------------------------------------------------------

-- ** Alternative append


-- | 'hAppend'' below is implemented using the same idea
append' :: [a] -> [a] -> [a]
append' l l' = foldr (:) l' l

-- | Alternative implementation of 'hAppend'. Demonstrates 'HFoldr'
hAppend' :: (HFoldr FHCons v l r) => HList l -> v -> r
hAppend' l l' = hFoldr FHCons l' l

data FHCons = FHCons

instance ( x ~ (e,HList l), y ~ (HList (e ': l))) => ApplyAB FHCons x y  where
    applyAB _ (e,l) = HCons e l


-- ** Historical append

{- $

The original HList code is included below. In both cases
we had to program the algorithm twice, at the term and the type levels.

[@The class HAppend@]

> class HAppend l l' l'' | l l' -> l''
>  where
>   hAppend :: l -> l' -> l''
>

[@The instance following the normal append@]

> instance HList l => HAppend HNil l l
>  where
>   hAppend HNil l = l
>
> instance (HList l, HAppend l l' l'')
>       => HAppend (HCons x l) l' (HCons x l'')
>  where
>   hAppend (HCons x l) l' = HCons x (hAppend l l')

-}

-- --------------------------------------------------------------------------
-- * Reversing HLists

-- Append the reversed l1 to l2
type family HRevApp (l1 :: [*]) (l2 :: [*]) :: [*]
type instance HRevApp '[] l = l
type instance HRevApp (e ': l) l' = HRevApp l (e ': l')

hRevApp :: HList l1 -> HList l2 -> HList (HRevApp l1 l2)
hRevApp HNil l = l
hRevApp (HCons x l) l' = hRevApp l (HCons x l')

hReverse l = hRevApp l HNil



-- --------------------------------------------------------------------------

--
-- * A nicer notation for lists
--


-- | List termination
hEnd :: HList l -> HList l
hEnd = id

{- ^
   Note:

        [@x :: HList a@] means: @forall a. x :: HList a@

        [@hEnd x@] means: @exists a. x :: HList a@
-}


-- |  Building lists

hBuild :: (HBuild' '[] r) => r
hBuild =  hBuild' HNil

class HBuild' l r where
    hBuild' :: HList l -> r

instance (l' ~ HRevApp l '[])
      => HBuild' l (HList l') where
  hBuild' l = hReverse l

instance HBuild' (a ': l) r
      => HBuild' l (a->r) where
  hBuild' l x = hBuild' (HCons x l)

-- ** examples
{- $examplesNote

The classes above allow the third (shortest) way to make a list
(containing a,b,c) in this case

> list = a `HCons` b `HCons` c `HCons` HNil
> list = a .*. b .*. c .*. HNil
> list = hEnd $ hBuild a b c

>>> let x = hBuild True in hEnd x
H[True]

>>> let x = hBuild True 'a' in hEnd x
H[True, 'a']

>>> let x = hBuild True 'a' "ok" in hEnd x
H[True, 'a', "ok"]

-}

-- *** historical
{- $hbuild the show instance has since changed, but these uses of
'hBuild'/'hEnd' still work

> HList> let x = hBuild True in hEnd x
> HCons True HNil

> HList> let x = hBuild True 'a' in hEnd x
> HCons True (HCons 'a' HNil)

> HList> let x = hBuild True 'a' "ok" in hEnd x
> HCons True (HCons 'a' (HCons "ok" HNil))

> HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5))
> HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))

> HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5)) == angus
> True

-}

-- --------------------------------------------------------------------------

-- * folds
-- ** foldr
-- $foldNote  Consume a heterogenous list. GADTs and type-classes mix well


class HFoldr f v (l :: [*]) r where
    hFoldr :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr f v '[] v' where
    hFoldr       _ v _   = v

-- | uses 'ApplyAB' not 'Apply'
instance (ApplyAB f (e, r) r', HFoldr f v l r)
    => HFoldr f v (e ': l) r' where
    hFoldr f v (HCons x l)    = applyAB f (x, hFoldr f v l :: r)


class HScanr f z ls rs where
    hScanr :: f -> z -> HList ls -> HList rs

instance HScanr f z '[] '[z] where
    hScanr _ z _ = HCons z HNil

instance (ApplyAB f (x,r) s, HScanr f z xs (r ': rs)) => HScanr f z (x ': xs) (s ': r ': rs) where
    hScanr f z (HCons x xs) =
        case hScanr f z xs :: HList (r ': rs) of
            HCons r rs -> (applyAB f (x,r) :: s) `HCons` r `HCons` rs

-- | always is Left. This is a workable hFoldr for ghc-7.7,
-- but it is very inconvenient
hFoldrFromHScanrTagged :: forall f z ls el e l. (HScanr f z ls el, el ~ (e ': l))
    => f -> z -> HList ls -> Either e l
hFoldrFromHScanrTagged  f z ls = Left (hHead (hScanr f z ls :: HList el) :: e)



-- ** foldl

{- | like 'foldl'


>>> hFoldl (uncurry $ flip (:)) [] (1 `HCons` 2 `HCons` HNil)
[2,1]


-}
class HFoldl f (z :: *) xs (r :: *) where
    hFoldl :: f -> z -> HList xs -> r

instance forall f z z' r x zx xs. (zx ~ (z,x), ApplyAB f zx z', HFoldl f z' xs r)
    => HFoldl f z (x ': xs) r where
    hFoldl f z (x `HCons` xs) = hFoldl f (applyAB f (z,x) :: z') xs

instance (z ~ z') => HFoldl f z '[] z' where
    hFoldl _ z _ = z





-- * unfold
-- $unfoldNote Produce a heterogenous list. Uses the more limited
-- 'Apply' instead of 'App' since that's all that is needed for uses of this
-- function downstream. Those could in principle be re-written.

hUnfold :: (Apply p s, HUnfold' p (ApplyR p s)) => p -> s -> HList (HUnfold p s)
hUnfold p s = hUnfold' p (apply p s)

type HUnfold p s = HUnfoldR p (ApplyR p s)

class HUnfold' p res where
    type HUnfoldR p res :: [*]
    hUnfold' :: p -> res -> HList (HUnfoldR p res)

instance HUnfold' p HNothing where
    type HUnfoldR p HNothing = '[]
    hUnfold' _ _ = HNil

instance (Apply p s, HUnfold' p (ApplyR p s)) => HUnfold' p (HJust (e,s)) where
    type HUnfoldR p (HJust (e,s)) = e ': HUnfold p s
    hUnfold' p (HJust (e,s)) = HCons e (hUnfold p s)


-- * replicate

class HReplicate (n :: HNat) e where
    type HReplicateR n e :: [*]
    hReplicate :: Proxy n -> e -> HList (HReplicateR n e)

instance HReplicate HZero e where
    type HReplicateR HZero e = '[]
    hReplicate _ _ = HNil

instance HReplicate n e => HReplicate (HSucc n) e where
    type HReplicateR (HSucc n) e = e ': HReplicateR n e
    hReplicate n e = e `HCons` hReplicate (hPred n) e



-- --------------------------------------------------------------------------
-- * traversing HLists

-- ** producing HList
-- *** map
-- $mapNote It could be implemented with 'hFoldr', as we show further below

{- | hMap is written such that the length of the result list
can be determined from the length of the argument list (and
the other way around). Similarly, the type of the elements
of the list is propagated in both directions too.

Excuse the ugly types printed. Unfortunately ghc (still?)
shows types like @'[a,b]@ using the actual constructors involved
@(':) a ((':) b '[])@ (or even worse when the kind variables are printed).

>>> :set -XNoMonomorphismRestriction
>>> let xs = 1 .*. 'c' .*. HNil
>>> :t hMap (HJust ()) xs
hMap (HJust ()) xs
  :: Num y => HList ((':) * (HJust y) ((':) * (HJust Char) ('[] *)))


These 4 examples show that the constraint on the length (2 in this cae)
can be applied before or after the 'hMap'. That inference is independent of the
direction that type information is propagated for the individual elements.


>>> let asLen2 xs = xs `asTypeOf` (undefined :: HList '[a,b])

>>> let lr xs = asLen2 (applyAB (HMap HRead) xs)
>>> let ls xs = asLen2 (applyAB (HMap HShow) xs)
>>> let rl xs = applyAB (HMap HRead) (asLen2 xs)
>>> let sl xs = applyAB (HMap HShow) (asLen2 xs)


>>> :t lr
lr
  :: (Read y, Read y1) =>
     HList ((':) * String ((':) * String ('[] *)))
     -> HList ((':) * y ((':) * y1 ('[] *)))

>>> :t rl
rl
  :: (Read y, Read y1) =>
     HList ((':) * String ((':) * String ('[] *)))
     -> HList ((':) * y ((':) * y1 ('[] *)))


>>> :t ls
ls
  :: (Show y, Show y1) =>
     HList ((':) * y ((':) * y1 ('[] *)))
     -> HList ((':) * String ((':) * String ('[] *)))

>>> :t sl
sl
  :: (Show y, Show y1) =>
     HList ((':) * y ((':) * y1 ('[] *)))
     -> HList ((':) * String ((':) * String ('[] *)))

-}

hMap f xs = applyAB (HMap f) xs

newtype HMap f = HMap f

instance (HMapCxt f as bs as' bs') => ApplyAB (HMap f) as bs where
    applyAB (HMap f) = hMapAux f

type HMapCxt f as bs as' bs' = (HMapAux f as' bs', as ~ HList as', bs ~ HList bs',
    SameLength as' bs', SameLength bs' as')


-- | Ensure two lists have the same length. We do case analysis on the
-- first one (hence the type must be known to the type checker).
-- In contrast, the second list may be a type variable.
class SameLength es1 es2
instance (es2 ~ '[]) => SameLength '[] es2
instance (SameLength xs ys, es2 ~ (y ': ys)) => SameLength (x ': xs) es2




class HMapAux f (l :: [*]) (r :: [*]) where
  hMapAux :: (SameLength l r, SameLength r l) => f -> HList l -> HList r

instance HMapAux f '[] '[] where
  hMapAux       _  _  = HNil

instance (ApplyAB f e e', HMapAux f l l',
    SameLength l l', SameLength l' l)
    => HMapAux f (e ': l) (e' ': l') where
  hMapAux f (HCons x l)    = applyAB f x `HCons` hMapAux f l




-- --------------------------------------------------------------------------

-- **** alternative implementation
-- $note currently broken

newtype MapCar f = MapCar f

-- | Same as 'hMap' only a different implementation.
hMapMapCar :: (HFoldr (MapCar f) (HList '[]) l l') =>
    f -> HList l -> l'
hMapMapCar f = hFoldr (MapCar f) HNil

instance ApplyAB f e e' => ApplyAB (MapCar f) (e,HList l) (HList (e' ': l)) where
    applyAB (MapCar f) (e,l) = HCons (applyAB f e) l


-- --------------------------------------------------------------------------

-- *** @appEndo . mconcat . map Endo@
{- |

>>> let xs = length .*. (+1) .*. (*2) .*. HNil
>>> hComposeList xs "abc"
8


-}
hComposeList
  :: (HFoldr Comp (a -> a) l (t -> a)) => HList l -> t -> a
hComposeList fs v0 = let r = hFoldr (undefined :: Comp) (\x -> x `asTypeOf` r) fs v0 in r


-- --------------------------------------------------------------------------

-- *** sequence
{- |
   A heterogeneous version of

   > sequenceA :: (Applicative m) => [m a] -> m [a]

   Only now we operate on heterogeneous lists, where different elements
   may have different types 'a'.
   In the argument list of monadic values (m a_i),
   although a_i may differ, the monad 'm' must be the same for all
   elements. That's why we needed "Data.HList.TypeCastGeneric2" (currently (~)).
   The typechecker will complain
   if we attempt to use hSequence on a HList of monadic values with different
   monads.

   The 'hSequence' problem was posed by Matthias Fischmann
   in his message on the Haskell-Cafe list on Oct 8, 2006

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018708.html>

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018784.html>
 -}

class (Applicative m, SameLength a b, SameLength b a) => HSequence m a b | a -> b, m b -> a where
    hSequence :: HList a -> m (HList b)
{- ^

[@Maybe@]

>>> hSequence $ Just (1 :: Integer) `HCons` (Just 'c') `HCons` HNil
Just H[1, 'c']

>>> hSequence $  return 1 `HCons` Just  'c' `HCons` HNil
Just H[1, 'c']


[@List@]

>>> hSequence $ [1] `HCons` ['c'] `HCons` HNil
[H[1, 'c']]


-}

instance Applicative m => HSequence m '[] '[] where
    hSequence _ = pure HNil

instance (m1 ~ m, Applicative m, HSequence m as bs) =>
    HSequence m (m1 a ': as) (a ': bs) where
    hSequence (HCons a b) = liftA2 HCons a (hSequence b)

-- data ConsM = ConsM
-- consM = LiftA2 FHCons
newtype LiftA2 f = LiftA2 f

instance (ApplyAB f (x,y) z, mz ~ m z, mxy ~ (m x, m y), Applicative m) => ApplyAB (LiftA2 f) mxy mz where
    applyAB (LiftA2 f) xy = liftA2 (curry (applyAB f)) `uncurry` xy

{-
instance (m1 ~ m, Applicative m) => ApplyAB ConsM (m a, m1 (HList l)) (m (HList (a ': l)))  where
{-
    type ApplyB ConsM (m a, m1 (HList l)) = Just (m (HList (a ': l)))
    type ApplyA ConsM (m (HList (a ': l))) = Just (m a, m (HList l))
    -}
    applyAB _ (me,ml) = liftA2 HCons me ml
    -}


-- **** alternative implementation

-- | 'hSequence2' is not recommended over 'hSequence' since it possibly doesn't
-- allow inferring argument types from the result types. Otherwise this version
-- should do exactly the same thing.
--
-- The DataKinds version needs a little help to find the type of the
-- return HNil, unlike the original version, which worked just fine as
--
--  > hSequence l = hFoldr ConsM (return HNil) l


{-
hSequence2 :: HSequence2 l f a => HList l -> f a
hSequence2 l =
    let rHNil = pure HNil `asTypeOf` (liftA undefined x)
        x = hFoldr ConsM rHNil l
    in x


-- | abbreviation for the constraint on 'hSequence2'
type HSequence2 l f a = (Applicative f, HFoldr ConsM (f (HList ('[]))) l (f a))
-}


-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- ** producing homogenous lists

-- *** map (no sequencing)
-- $mapOut This one we implement via hFoldr

newtype Mapcar f = Mapcar f

instance (l ~ [e'], ApplyAB f e e', el ~ (e,l)) => ApplyAB (Mapcar f) el l where
    applyAB (Mapcar f) (e, l) = applyAB f e : l

-- A synonym for the complex constraint
type HMapOut f l e = (HFoldr (Mapcar f) [e] l [e])

hMapOut :: forall f e l. HMapOut f l e => f -> HList l -> [e]
hMapOut f l = hFoldr (Mapcar f) ([] :: [e]) l


-- --------------------------------------------------------------------------
-- *** mapM

-- |
--
-- > mapM :: forall b m a. (Monad m) => (a -> m b) -> [a] -> m [b]
--
-- Likewise for mapM_.
--
-- See 'hSequence' if the result list should also be heterogenous.

hMapM   :: (Monad m, HMapOut f l (m e)) => f -> HList l -> [m e]
hMapM f =  hMapOut f

-- | GHC doesn't like its own type.
-- hMapM_  :: forall m a f e. (Monad m, HMapOut f a (m e)) => f -> a -> m ()
-- Without explicit type signature, it's Ok. Sigh.
-- Anyway, Hugs does insist on a better type. So we restrict as follows:
--
hMapM_   :: (Monad m, HMapOut f l (m ())) => f -> HList l -> m ()
hMapM_ f =  sequence_ .  disambiguate . hMapM f
 where
  disambiguate :: [q ()] -> [q ()]
  disambiguate =  id





-- --------------------------------------------------------------------------
-- * Type-level equality for lists ('HEq')

instance HEq '[] '[]      True
instance HEq '[] (e ': l) False
instance HEq (e ': l) '[] False
instance (HEq e1 e2 b1, HEq l1 l2 b2, br ~ HAnd b1 b2)
      => HEq (e1 ': l1) (e2 ': l2) br

-- --------------------------------------------------------------------------
-- * Ensure a list to contain HNats only
-- | We do so constructively, converting the HList whose elements
-- are Proxy HNat to [HNat]. The latter kind is unpopulated and
-- is present only at the type level.

type family HNats (l :: [*]) :: [HNat]
type instance HNats '[] = '[]
type instance HNats (Proxy n ': l) = n ': HNats l

hNats :: HList l -> Proxy (HNats l)
hNats = undefined


-- --------------------------------------------------------------------------
-- * Membership tests

-- | Check to see if an HList contains an element with a given type
-- This is a type-level only test

class HMember e1 (l :: [*]) (b :: Bool) | e1 l -> b
instance HMember e1 '[] False
instance (HEq e1 e b, HMember' b e1 l br) => HMember  e1 (e ': l) br
class HMember' (b0 :: Bool) e1 (l :: [*]) (b :: Bool) | b0 e1 l -> b
instance HMember' True e1 l True
instance (HMember e1 l br) => HMember' False e1 l br

-- The following is a similar type-only membership test
-- It uses the user-supplied curried type equality predicate pred
type family HMemberP pred e1 (l :: [*]) :: Bool
type instance HMemberP pred e1 '[] = False
--type instance HMemberP pred e1 (e ': l) = HMemberP' pred e1 l (ApplyR pred (e1,e))

type family HMemberP' pred e1 (l :: [*]) pb :: Bool
type instance HMemberP' pred e1 l (Proxy True) = True
type instance HMemberP' pred e1 l (Proxy False) = HMemberP pred e1 l
 

hMember :: HMember e l b => e -> HList l -> Proxy b
hMember = undefined

-- ** Another type-level membership test
--
-- | Check to see if an element e occurs in a list l
-- If not, return 'Nothing
-- If the element does occur, return 'Just l1
-- where l1 is a type-level list without e
class HMemberM (e1 :: k) (l :: [k]) (r :: Maybe [k]) | e1 l -> r
instance HMemberM e1 '[] 'Nothing
instance (HEq e1 e b, HMemberM1 b e1 (e ': l) res)
      =>  HMemberM e1 (e ': l) res

class HMemberM1 (b::Bool) (e1 :: k) (l :: [k]) (r::Maybe [k]) | b e1 l -> r
instance HMemberM1 True e1 (e ': l) ('Just l)
instance (HMemberM e1 l r, HMemberM2 r e1 (e ': l) res)
    => HMemberM1 False e1 (e ': l) res

class HMemberM2 (b::Maybe [k]) (e1 :: k) (l :: [k]) (r::Maybe [k]) | b e1 l -> r
instance HMemberM2 Nothing e1 l Nothing
instance HMemberM2 (Just l1) e1 (e ': l) (Just (e ': l1))

{-
-- --------------------------------------------------------------------------

-- * Staged equality for lists

instance HStagedEq HNil HNil
 where
  hStagedEq _ _ = True

instance HStagedEq HNil (HCons e l)
 where
  hStagedEq _ _ = False

instance HStagedEq (HCons e l) HNil
 where
  hStagedEq _ _ = False

instance ( TypeEq e e' b
         , HStagedEq l l'
         , HStagedEq' b e e'
         )
      =>   HStagedEq (HCons e l) (HCons e' l')
 where
  hStagedEq (HCons e l) (HCons e' l') = (hStagedEq' b e e') && b'
   where
    b  = typeEq e e'
    b' = hStagedEq l l'

class HStagedEq' b e e'
 where
  hStagedEq' :: b -> e -> e' -> Bool

instance HStagedEq' HFalse e e'
 where
  hStagedEq' _ _ _ = False

instance Eq e => HStagedEq' HTrue e e
 where
  hStagedEq' _ = (==)




-- * Static set property based on HEq
class HSet l
instance HSet HNil
instance (HMember e l HFalse, HSet l) => HSet (HCons e l)
-}

-- * Find an element in a set based on HEq
-- | It is a pure type-level operation
-- XXX should be poly-kinded
class HFind (e :: *) (l :: [*]) (n :: HNat) | e l -> n

instance (HEq e1 e2 b, HFind' b e1 l n) => HFind e1 (e2 ': l) n

class HFind' (b::Bool) (e :: *) (l::[*]) (n::HNat) | b e l -> n
instance HFind' True e l HZero
instance HFind e l n => HFind' False e l (HSucc n)



-- ** Membership test based on type equality

-- | could be an associated type if HEq had one
class HTMember e (l :: [*]) (b :: Bool) | e l -> b
instance HTMember e '[] False
instance (HEq e e' b, HTMember e l b', HOr b b' ~ b'')
      =>  HTMember e (e' ': l) b''

hTMember :: HTMember e l b => e -> HList l -> Proxy b
hTMember _ _ = proxy


-- * Intersection based on HTMember

class HTIntersect l1 l2 l3 | l1 l2 -> l3
 where
  -- | Like 'Data.List.intersect'
  hTIntersect :: HList l1 -> HList l2 -> HList l3

instance HTIntersect '[] l '[]
 where
  hTIntersect _ _ = HNil

instance ( HTMember h l1 b
         , HTIntersectBool b h t l1 l2
         )
         => HTIntersect (h ': t) l1 l2
 where
  hTIntersect (HCons h t) l1 = hTIntersectBool b h t l1
   where
    b = hTMember h l1

class HTIntersectBool (b :: Bool) h t l1 l2 | b h t l1 -> l2
 where
 hTIntersectBool :: Proxy b -> h -> HList t -> HList l1 -> HList l2

instance HTIntersect t l1 l2
      => HTIntersectBool True h t l1 (h ': l2)
 where
  hTIntersectBool _ h t l1 = HCons h (hTIntersect t l1)

instance HTIntersect t l1 l2
      => HTIntersectBool False h t l1 l2
 where
  hTIntersectBool _ _ t l1 = hTIntersect t l1


-- * Turn a heterogeneous list into a homogeneous one

-- | Same as @hMapOut Id@
class HList2List l e
 where
  hList2List :: HList l -> [e]

instance HList2List '[] e
 where
  hList2List HNil = []

instance HList2List l e
      => HList2List (e ': l) e
 where
  hList2List (HCons e l) = e:hList2List l




-- --------------------------------------------------------------------------
-- * With 'HMaybe'

-- ** Turn list in a list of justs
-- | the same as @map Just@
--
-- >>> toHJust (2 .*. 'a' .*. HNil)
-- H[HJust 2, HJust 'a']
--
-- >>> toHJust2 (2 .*. 'a' .*. HNil)
-- H[HJust 2, HJust 'a']

class ToHJust l l' | l -> l', l' -> l
 where
  toHJust :: HList l -> HList l'

instance ToHJust '[] '[]
 where
  toHJust HNil = HNil

instance ToHJust l l' => ToHJust (e ': l) (HJust e ': l')
 where
  toHJust (HCons e l) = HCons (HJust e) (toHJust l)

-- | alternative implementation. The Apply instance is in "Data.HList.FakePrelude".
-- A longer type could be inferred.
-- toHJust2 :: (HMap' (HJust ()) a b) => HList a -> HList b
toHJust2 xs = hMap (HJust ()) xs

-- --------------------------------------------------------------------------
-- ** Extract justs from list of maybes
--
-- >>> let xs = 2 .*. 'a' .*. HNil
-- >>> fromHJust (toHJust xs) == xs
-- True

class FromHJust l
 where
  type FromHJustR l :: [*]
  fromHJust :: HList l -> HList (FromHJustR l)

instance FromHJust '[]
 where
  type FromHJustR '[] = '[]
  fromHJust HNil = HNil

instance FromHJust l => FromHJust (HNothing ': l)
 where
  type FromHJustR (HNothing ': l) = FromHJustR l
  fromHJust (HCons _ l) = fromHJust l

instance FromHJust l => FromHJust (HJust e ': l)
 where
  type FromHJustR (HJust e ': l) = e ': FromHJustR l
  fromHJust (HCons (HJust e) l) = HCons e (fromHJust l)

-- *** alternative implementation

-- | A longer type could be inferred.
-- fromHJust2 :: (HMap' HFromJust a b) => HList a -> HList b
fromHJust2 xs = hMap HFromJust xs

data HFromJust = HFromJust
instance (hJustA ~ HJust a) => ApplyAB HFromJust hJustA a where
    applyAB _ (HJust a) = a


-- --------------------------------------------------------------------------
-- * Annotated lists

data HAddTag t = HAddTag t
data HRmTag    = HRmTag

-- hAddTag :: HMap' (HAddTag t) l r => t -> HList l -> HList r
hAddTag t l = hMap (HAddTag t) l

-- hRmTag ::  HMap HRmTag l => HList l -> HList (HMapR HRmTag l)
hRmTag l    = hMap HRmTag l

instance (et ~ (e,t)) => ApplyAB (HAddTag t) e et
 where
  applyAB (HAddTag t) e = (e,t)


instance (e' ~ e) => ApplyAB HRmTag (e,t) e'
 where
  applyAB _ (e,_) = e


-- | Annotate list with a type-level Boolean
-- hFlag :: HMap' (HAddTag (Proxy True)) l r => HList l -> HList r
hFlag l = hAddTag hTrue l


-- --------------------------------------------------------------------------
-- * Splitting by HTrue and HFalse

-- | Analogus to Data.List.'Data.List.partition' 'snd'
--
-- >>> hSplit $ (2,hTrue) .*. (3,hTrue) .*. (1,hFalse) .*. HNil
-- (H[2, 3],H[1])
--
-- it might make more sense to instead have @LVPair Bool e@
-- instead of @(e, Proxy Bool)@ since the former has the same
-- runtime representation as @e@

class HSplit l
 where
  type HSplitT l :: [*]
  type HSplitF l :: [*]
  hSplit :: HList l -> (HList (HSplitT l), HList (HSplitF l))

instance HSplit '[]
 where
  type HSplitT '[] = '[]
  type HSplitF '[] = '[]
  hSplit HNil = (HNil,HNil)

instance HSplit l => HSplit ((e, Proxy True) ': l)
 where

  type HSplitT ((e,Proxy True) ': l) = e ': HSplitT l
  type HSplitF ((e,Proxy True) ': l) = HSplitF l

  hSplit (HCons (e,_) l) = (HCons e l',l'')
   where
    (l',l'') = hSplit l

instance HSplit l => HSplit ((e,Proxy False) ': l)
 where
  type HSplitT ((e,Proxy False) ': l) = HSplitT l
  type HSplitF ((e,Proxy False) ': l) = e ': HSplitF l

  hSplit (HCons (e,_) l) = (l',HCons e l'')
   where
    (l',l'') = hSplit l

{-

Let expansion makes a difference to Hugs:

HListPrelude> let x = (hFlag (HCons "1" HNil)) in hSplit x
(HCons "1" HNil,HNil)
HListPrelude> hSplit (hFlag (HCons "1" HNil))
ERROR - Unresolved overloading
*** Type       : HSplit (HCons ([Char],HTrue) HNil) a b => (a,b)
*** Expression : hSplit (hFlag (HCons "1" HNil))


-}
