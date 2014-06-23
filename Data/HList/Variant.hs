

{- |
   Description: Variants, i.e., labelled sums, generalizations of Either

   The HList library

   See "Data.HList.CommonMain#Variant" for the public (safe) interface.

   The implementation here follows "Data.Dynamic", though Typeable is not
   needed.

   See @broken/VariantP.hs@ and @broken/VariantOld.hs@ for different approaches
   to open sums.
-}

module Data.HList.Variant where

import Data.HList.FakePrelude
import Data.HList.Record
import Data.HList.RecordPuns -- for doctest
import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.HArray

import Text.ParserCombinators.ReadP hiding (optional)

import Unsafe.Coerce
import GHC.Prim (Any)

import Control.Applicative
import LensDefs

-- * Labels for doctests

{- $setup

>>> let x = Label :: Label "x"
>>> let y = Label :: Label "y"
>>> let z = Label :: Label "z"
>>> let _left = Label :: Label "left"
>>> let _right = Label :: Label "right"

>>> :set -XQuasiQuotes -XViewPatterns -XDataKinds


-- * Creating Variants

It is necessary to specify the order in which the fields occur, using
a data type like

>>> let p = Proxy :: Proxy '[Tagged "left" Char, Tagged "right" Int]

Then this argument can be passed into 'mkVariant'

>>> let v = mkVariant _left 'x' p
>>> let w = mkVariant _right 5  p

>>> :t v
v :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w
w :: Variant '[Tagged "left" Char, Tagged "right" Int]


>>> [v,w]
[V{left='x'},V{right=5}]

-}


-- ** Alternative: a 'Record' as the Proxy
{- $mkVariant2

The type of mkVariant also allows using a 'Record' as the proxy. For example:

>>> :{
let p2 = [pun| left right |] where
            left = 'a'
            right = (4::Int)
:}

>>> let v2 = mkVariant _left 'x' p2
>>> let w2 = mkVariant _right 5  p2

>>> :t v2
v2 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w2
w2 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> (v2,w2)
(V{left='x'},V{right=5})

-}

-- ** A polymorphic Proxy
{- $mkVariant3

It is also possible to leave the @Char@ and @Int@ as type variables,
and have them inferred.

>>> let p3 = Proxy :: Proxy '[Tagged "left" a, Tagged "right" b]

Using @p3@ takes some care. The following attempt shows the problem:

>>> :{
let v3' = mkVariant _left 'x' p3
    w3' = mkVariant _right (5::Int) p3
:}

>>> :t v3'
v3' :: Variant '[Tagged "left" Char, Tagged "right" b]

>>> :t w3'
w3' :: Variant '[Tagged "left" a, Tagged "right" Int]

Here each use of @p3@ does not constrain the type of the other use.
In some cases those type variables will be inferred from other constraints,
such as when putting the variants into a list

>>> [v3', w3']
[V{left='x'},V{right=5}]

In other cases the other tags will be defaulted to (), at least if `ExtendedDefaultRules` is enabled:

>>> v3'
V{left='x'}

>>> :set -XNoExtendedDefaultRules
>>> let doctestCleanActual x = case x of "" -> x; _ -> (lines x !! 2) ++ "\n"
>>> v3'
    No instance for (Show b0) arising from a use of ‘print’

>>> let doctestCleanActual x = x


Another way around this issue is to make sure that the proxy
is bound in a monomorphic pattern. These are patterns that allow
name shadowing.

* @\p -> ...@
* @case e of p -> ...@
* @do p <- e; ...@
* implicit parameters @let ?p = e in ...@
* <http://stackoverflow.com/questions/23899279#23899611 other patterns involved in mutually recursive bindings>

An example of the case:

>>> :{
let (v3,w3) = case p3 of
              p -> (mkVariant _left 'x' p,
                    mkVariant _right (5 :: Int) p)
:}


>>> :t v3
v3 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w3
w3 :: Variant '[Tagged "left" Char, Tagged "right" Int]

-}

-- --------------------------------------------------------------------------
{- |
@Variant vs@ has an implementation similar to 'Dynamic', except the
contained value is one of the elements of the @vs@ list, rather than
being one particular instance of 'Typeable'.

>>> v .!. _right
Nothing

>>> v .!. _left
Just 'x'

In some cases the 'pun' quasiquote works with variants,

>>> let f [pun| left right |] = (left,right)
>>> f v
(Just 'x',Nothing)

>>> f w
(Nothing,Just 5)


>>> let add1 v = hMapV (Fun succ :: Fun '[Enum] '()) v

>>> f (add1 v)
(Just 'y',Nothing)

>>> f (add1 w)
(Nothing,Just 6)


-}
data Variant (vs :: [*]) = Variant !Int Any

-- ** Unsafe operations

-- | This is only safe if the n'th element of vs has type @Tagged t v@
unsafeMkVariant :: Int -- ^ n
                -> v
                -> Variant vs
unsafeMkVariant n a = Variant n (unsafeCoerce a)

{- | Safe when (e ~ e') given that

> Tagged t e ~ HLookupByHNatR n v
> Tagged t' e' ~ HLookupByHNatR n v'

'hUpdateAtLabel' is the safe version

-}
unsafeCastVariant :: Variant v -> Variant v'
unsafeCastVariant (Variant n e) = Variant n e

-- | private destructor. This is safe only if the value
-- contained actually has type `e`
unsafeUnVariant :: Variant v -> e
unsafeUnVariant (Variant _ e) = unsafeCoerce e


-- --------------------------------------------------------------------------
-- * Public constructor

class HasField x (Variant vs) (Maybe v) =>
      MkVariant x v vs | x vs -> v where
    mkVariant :: Label x -- ^ the tag
        -> v -- ^ value to be stored
        -> proxy vs -- ^ a helper to fix the ordering and types of the
                    -- potential values that this variant contains.
                    -- Typically this will be a 'Proxy', 'Record' or
                    -- another 'Variant'
        -> Variant vs

emptyVariant :: Variant '[]
emptyVariant = unsafeMkVariant 0 ()

instance (HFindLabel x vs n,
          HNat2Integral n,
          HasField x (Variant vs) (Maybe v)) =>
    MkVariant x v vs where
  mkVariant _x y _p = unsafeMkVariant (hNat2Integral (Proxy :: Proxy n)) y
  -- done as a one-instance class instead of a function to be able to hide
  -- the 'n' type variable

-- --------------------------------------------------------------------------
-- * public destructor

{- $note 'hLookupByLabel' (synonym '.!.')

> (.!.)             :: Variant v -> Label x -> Maybe e
> hLookupByLabel    :: Label x -> Variant v -> Maybe e

'hPrism' and 'hLens'' combine this with 'mkVariant'
-}
instance (HasField x (Record vs) a,
          HFindLabel x vs n,
          HNat2Integral n)
  => HasField x (Variant vs) (Maybe a) where
  hLookupByLabel _x (Variant n d)
          | hNat2Integral (Proxy :: Proxy n) == n = Just (unsafeCoerce d)
          | otherwise = Nothing


-- --------------------------------------------------------------------------
-- * prism

{- | Make a @Prism (Variant s) (Variant t) a b@ out of a Label.

See "Data.HList.Labelable".'hLens'' is a more overloaded version.

Few type annotations are necessary because of the restriction
that `s` and `t` have the same labels in the same order, and to
get \"t\" the \"a\" in \"s\" is replaced with \"b\".

-}
class (Choice p, Applicative f, SameLength s t)
        => HPrism x p f s t a b
          | x s -> a, x t -> b,    -- lookup
            x s b -> t, x t a -> s -- update
  where
    hPrism :: Label x -> p a (f b) -> p (Variant s) (f (Variant t))


instance (
    MkVariant x b t,

    HasField x (Variant s) (Maybe a),

    -- labels in the HList are not changed at all:
    -- number, ordering, actual values are all constant
    SameLength s t,
    HMapCxt Record (Fun '[] ()) s s_,
    HMapCxt Record (Fun '[] ()) t t_,
    s_ ~ t_,

    -- only the target of the prism can have it's type changed
    H2ProjectByLabels '[Label x] s si so,
    H2ProjectByLabels '[Label x] t ti to,
    so ~ to,

    -- to convince GHC the fundeps are satisfied
    HUpdateAtLabel Variant x b s t,
    HUpdateAtLabel Variant x a t s,

    -- required based on how definition of 'prism' works
    Choice p,
    Applicative f
   ) => HPrism x p f s t a b where
    hPrism x = prism (\b -> mkVariant x b Proxy)
                  (\s -> case hLookupByLabel x s of
                    Just a -> Right a
                    Nothing -> Left (unsafeCastVariant s :: Variant t))



-- --------------------------------------------------------------------------
-- * Read
-- | Variants are not opaque
instance (ShowVariant vs) => Show (Variant vs) where
    showsPrec _ v = ("V{"++) . showVariant v . ('}':)


-- | helper class for defining the Show instance
class ShowVariant vs where
    showVariant :: Variant vs -> ShowS

instance (ShowLabel l, Show v, ShowVariant vs)
      => ShowVariant (Tagged l v ': vs) where
    showVariant vs | Just v <- hLookupByLabel l vs = \rest ->
                            showLabel l ++ "=" ++ show v ++ rest
      where l = Label :: Label l
    showVariant (Variant n v) = showVariant (Variant (n-1) v :: Variant vs)

instance ShowVariant '[] where
    showVariant = error "empty variant: invariant broken"

-- --------------------------------------------------------------------------
-- * Show
-- | A corresponding read instance

instance ReadVariant v => Read (Variant v) where
    readsPrec _ = readP_to_S $ do
      _ <- string "V{"
      r <- readVariant
      _ <- string "}"
      return r

class ReadVariant vs where
    readVariant :: ReadP (Variant vs)

instance ReadVariant '[] where
    readVariant = return unsafeEmptyVariant

instance (ShowLabel l, Read v, ReadVariant vs)
    => ReadVariant (Tagged l v ': vs) where
    readVariant = do
      mlv <- optional lv
      case mlv of
        Nothing -> do
          rest <- readVariant
          return (l .=. mlv .*. rest)
        Just e -> do
          return (mkVariant l e p)

      where
        lv = do
            _ <- string (showLabel l)
            _ <- string "="
            readS_to_P reads

        l = Label :: Label l

        p = Proxy :: Proxy (Tagged l v ': vs)

-- --------------------------------------------------------------------------
-- * Map
-- Apply a function to all possible elements of the variant
newtype HMapV f = HMapV f

-- | shortcut for @applyAB . HMapV@
hMapV f v = applyAB (HMapV f) v

-- | apply a function to all tags of the variant.
instance (vx ~ Variant x,
          vy ~ Variant y,
          HMapAux Variant (HFmap f) x y,
          SameLength x y)
     => ApplyAB (HMapV f) vx vy where
    applyAB (HMapV f) x = hMapAux (HFmap f) x

instance HMapAux Variant f '[] '[] where
    hMapAux _ _ = error "HMapVAux: variant invariant broken"

instance (ApplyAB f e e', HMapAux Variant f l l', SameLength l l')
    => HMapAux Variant f (e ': l) (e' ': l') where

      hMapAux f (Variant 0 v) = unsafeMkVariant 0 (applyAB f (toE v) :: e')
            where toE :: Any -> e
                  toE = unsafeCoerce

      hMapAux f (Variant n v) = cons (hMapAux f (Variant (n-1) v :: Variant l))
            where cons :: Variant l' -> Variant (e' ': l')
                  cons (Variant m e) = Variant (m+1) e

-- --------------------------------------------------------------------------
-- * HUpdateAtLabel instance

{- |

> hUpdateAtLabel x e' (mkVariant x e proxy) == mkVariant x e' proxy
> hUpdateAtLabel y e' (mkVariant x e proxy) == mkVariant x e  proxy

-}
instance
   (HUpdateVariantAtLabelCxt l e v v' n _e) =>
    HUpdateAtLabel Variant l e v v' where
    hUpdateAtLabel l e v = case hLookupByLabel l v of
          Just _e -> mkVariant l e (Proxy :: Proxy v')
          Nothing -> unsafeCastVariant v

type HUpdateVariantAtLabelCxt l e v v' n _e =
   (HFindLabel l v n,
    HFindLabel l v' n,
    HUpdateAtHNatR n (Tagged l e) v ~ v',
    HasField l (Variant v) (Maybe _e),
    HasField l (Record v') e,
    MkVariant l e v')


-- --------------------------------------------------------------------------
-- * HExtend instance
{- | Extension for Variants prefers the first value

> (l .=. Nothing) .*. v = v
> (l .=. Just e)  .*. _ = mkVariant l e Proxy

-}
instance (le ~ Tagged l (Maybe e)) =>
    HExtend le (Variant v) where
    type HExtendR le (Variant v) = Variant (UnMaybe le ': v)
    Tagged (Just e) .*. _ = unsafeMkVariant 0 e
    Tagged Nothing .*. (Variant n e) = Variant (n+1) e

type family UnMaybe le
type instance UnMaybe (Tagged l (Maybe e)) = Tagged l e


-- --------------------------------------------------------------------------
-- * Conversion to an untagged value
class HAllEqVal (x :: [*]) (b :: Bool) | x -> b
instance HAllEqVal '[] True
instance HAllEqVal '[x] True
instance (HEq a a' b,
          HAllEqVal (Tagged t a' ': xs) b2,
          HAnd b b2 ~ b3) =>
  HAllEqVal (Tagged s a ': Tagged t a' ': xs) b3


class HAllEqVal' (x :: [*])
instance HAllEqVal' '[]
instance HAllEqVal' '[x]
instance (HAllEqVal' (ta ': xs),
          a' ~ a,
          ta ~ Tagged t a,
          ta' ~ Tagged t' a')
  => HAllEqVal' (ta' ': ta ': xs)


{- | Similar to 'unvariant', except type variables in @v@
will be made equal to @e@ if possible. That allows the type
of @Nothing@ to be inferred as @Maybe Char@.

>>> unvariant' $ x .=. Nothing .*. mkVariant1 y 'y'
'y'

However, this difference leads to more local error messages
(@Couldn't match type ‘()’ with ‘Char’@), rather than the following
with @unvariant@:

> Fail
>    '("Variant",
>      '[Tagged "left" Char, Tagged "right" ()],
>      "must have all values equal to ",
>      e))

-}
unvariant' :: (HAllEqVal' (Tagged () e ': v),
               Unvariant v e)
  => Variant v -> e
unvariant' = unvariant

{- | Convert a Variant which has all possibilities having the same type
into a value of that type. Analogous to @either id id@.

See also 'unvariant'' -}
class Unvariant v e | v -> e where
    unvariant :: Variant v -> e

instance (Unvariant1 b v e,
          HAllEqVal v b,
          HAllEqVal (Tagged () e ': v) b)
    => Unvariant v e where
      unvariant = unvariant1 (Proxy :: Proxy b)


class Unvariant1 b v e | b v -> e where
    unvariant1 :: Proxy b -> Variant v -> e

instance (v ~ Tagged t1 e)
    => Unvariant1 True (v ': vs) e where
    unvariant1 _ = unsafeUnVariant

instance Fail '("Variant", v ': vs, "must have all values equal to ", e)
      => Unvariant1 False (v ': vs) Void where
    unvariant1 _ = error "Data.HList.Variant.Unvariant1 Fail must have no instances"

instance Fail "Unvariant applied to empty variant"
      => Unvariant1 b '[] Void where
    unvariant1 _ = error "Data.HList.Variant.Unvariant1 Fail must have no instances"


-- * zip

{- | Applies to variants that have the same labels
in the same order. A generalization of

> zipEither :: Either a b -> Either a b -> Maybe (Either (a,a) (b,b))
> zipEither (Left a) (Left a') = Just (Left (a,a'))
> zipEither (Right a) (Right a') = Just (Right (a,a'))
> zipEither _ _ = Nothing

-}
class ZipVariant x y xy | x y -> xy, xy -> x y where
    zipVariant :: Variant x -> Variant y -> Maybe (Variant xy)

instance ZipVariant '[] '[] '[] where
    zipVariant _ _ = Nothing

instance (tx ~ Tagged t x,
          ty ~ Tagged t y,
          txy ~ Tagged t (x,y),
          ZipVariant xs ys zs,
          MkVariant t (x,y) (txy ': zs))
  => ZipVariant (tx ': xs) (ty ': ys) (txy ': zs) where
    zipVariant x y = case (splitVariant x, splitVariant y) of
        (Left x', Left y') -> Just (mkVariant (Label :: Label t) (x',y') Proxy)
        (Right x', Right y') -> extendVariant <$> zipVariant x' y'
        _ -> Nothing


-- * Eq
instance (ZipVariant v v vv,
          HMapCxt Variant (HFmap UncurryEq) vv bools,
          Unvariant bools Bool) => Eq (Variant v)
  where
    v == v' = maybe
                False
                (\vv -> unvariant (hMapV UncurryEq vv :: Variant bools))
               $ zipVariant v v'


data UncurryEq = UncurryEq

instance (ee ~ (e,e), Eq e, bool ~ Bool) =>
    ApplyAB UncurryEq ee bool where
      applyAB _ (e,e') = e == e'
