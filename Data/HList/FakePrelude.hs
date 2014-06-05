{-# LANGUAGE CPP #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.
-}

module Data.HList.FakePrelude
    (module Data.HList.FakePrelude,
     module Data.Proxy) where

import Data.Proxy
import Data.Tagged
import GHC.Prim (Constraint)
import GHC.TypeLits
import Control.Applicative
#if NEW_TYPE_EQ
import Data.Type.Equality (type (==))
#endif




-- --------------------------------------------------------------------------
-- * A heterogeneous apply operator

-- | simpler/weaker version where type information only propagates forward
-- with this one. 'applyAB' defined below, is more complicated / verbose to define,
-- but it offers better type inference. Most uses have been converted to
-- 'applyAB', so there is not much that can be done with 'Apply'.
class Apply f a where
  type ApplyR f a :: *
  apply :: f -> a -> ApplyR f a

{- $note

 Polymorphic functions are not first-class in haskell. An example of this
 is:

 > f op = (op (1 :: Double), op (1 :: Int))

 [@RankNTypes@]

 One solution is to enable `-XRankNTypes` and then write a type
 signature which might be `f :: (forall a. Num a => a -> a)`. This
 does not work in the context of HList, since we want to use functions
 that do not necessarily fall into the pattern of (forall a. c a => a -> a).

 [@MultipleArguments@]

 Another solution is to rewrite @op@ to look like

 > f op1 op2 = (op1 (1:: Double), op2 (1 :: Int))

 In some sense this approach works (see HZip), but the result
 is constrained to as many function applications as you are willing to
 write (ex. a function that works for records of six entries would
 look like @hBuild f f f f f f@).


 [@Defunctionalization@]

 Therefore the selected solution is to write an instance of 'ApplyAB' for a data
 type that takes the place of the original function. In other words,

 > data Fn = Fn
 > instance ApplyAB Fn a b where applyAB Fn a = actual_fn a

 Normally you would have been able to pass around the definition actual_fn.

 [@Type inference / Local functional dependencies@]

 Note that @class ApplyAB@ has three parameters and no functional dependencies.
 Instances should be written in the style:

 > instance (int ~ Int, double ~ Double) => ApplyAB Fn int double
 >  where applyAB _ = fromIntegral

 rather than the more natural

 > instance ApplyAB Fn Int Double

 The first instance allows types to be inferred as if we had
 @class ApplyAB a b c | a -> b c@, while the second instance
 only matches if ghc already knows that it needs
 @ApplyAB Fn Int Double@. Since @applyAB Fn :: Int -> Double@
 has a monomorphic type, this trimmed down example does not
 really make sense because @applyAB (fromIntegral :: Int -> Double)@
 is exactly the same. Nontheless, the other uses of @ApplyAB@
 follow this pattern, and the benefits are seen when the type of
 @applyAB Fn@ has at least one type variable.

 Additional explanation can be found
 in <http://okmij.org/ftp/Haskell/typecast.html#local-fd local functional dependencies>


 [@AmbiguousTypes@]

 Note that ghc only allows AllowAmbiguousTypes when a type
 signature is provided. Thus expressions such as:

 > data AddJust = AddJust
 > instance (y ~ Maybe x) => ApplyAB AddJust x y where
 >    applyAB _ x = Just x
 >
 > twoJustsBad = hMap AddJust . hMap AddJust -- ambiguous type

 Are not accepted without a type signature that references the
 intermediate \"b\":

 > twoJusts :: forall r a b c. (HMapCxt r AddJust a b, HMapCxt r AddJust b c) =>
 >        r a -> r c
 > twoJusts a = hMap AddJust (hMap AddJust a :: r b)

 An apply class with functional dependencies

 > class ApplyAB' f a b | f a -> b, f b -> a

 Or with equivalent type families

 > class (GetB f a ~ b, GetA f b ~ a) => ApplyAB' f a b

 would not require an annotation for @twoJusts@. However, 
 not all instances of ApplyAB will satisfy those functional
 dependencies, and thus the number of classes would proliferate.
 Furthermore, inference does not have to be in one direction
 only, as the example of 'HMap' shows.

-}

-- | No constraints on result and argument types
class ApplyAB f a b where
  applyAB :: f -> a -> b


{- $fun

 'Fun' can be used instead of writing a new instance of
 'ApplyAB'. Refer to the definition/source for the the most
 concise explanation. A more wordy explanation is given below:

 A type signature needs to be provided on 'Fun' to make it work.
 Depending on the kind of the parameters to 'Fun', a number of
 different results happen.


 [@ex1@]

 A list of kind @[* -> Constraint]@ produces those
 constraints on the argument type:

 >>> :set -XDataKinds
 >>> let plus1 = Fun (\x -> if x < 5 then x+1 else 5) :: Fun '[Num, Ord] '()
 >>> :t applyAB plus1
 applyAB plus1 :: (Ord a, Num a) => a -> a

 Also note the use of @'()@ to signal that the result
 type is the same as the argument type.


 A single constraint can also be supplied:

 >>> let succ1 = Fun succ :: Fun Enum '()
 >>> :t applyAB succ1
 applyAB succ1 :: Enum a => a -> a


 >>> let just = Fun Just :: Fun '[] Maybe
 >>> :t applyAB just
 applyAB just :: a -> Maybe a


-}
data Fun (cxt :: k1) (getb :: k2)
    = Fun (forall a. FunCxt cxt a => a -> FunApp getb a)

{- | see 'Fun'. The only difference here is that the argument
type is calculated from the result type.

 >>> let rd = Fun' read :: Fun' Read String
 >>> :t applyAB rd
 applyAB rd :: Read b => [Char] -> b

 >>> let fromJust' = Fun' (\(Just a) -> a) :: Fun' '[] Maybe
 >>> :t applyAB fromJust'
 applyAB fromJust' :: Maybe b -> b

Note this use of Fun' means we don't have to get the b out of @Maybe b@,


-}
data Fun' (cxt :: k1) (geta :: k2)
    = Fun' (forall b. FunCxt cxt b => FunApp geta b -> b)


type family FunApp (fns :: k) a

type instance FunApp (fn :: *) a = fn
type instance FunApp (fn :: * -> *) a = fn a
type instance FunApp (fn :: ()) a = a

type family FunCxt (cxts :: k) a :: Constraint
type instance FunCxt (x ': xs) a = (x a, FunCxt xs a)
type instance FunCxt (cxt :: * -> Constraint) a = cxt a
type instance FunCxt '[] a = ()
-- | should there be so many ways to write no constraint?
type instance FunCxt (cxt :: ()) a = ()
type instance FunCxt (cxt :: *) a = (cxt ~ a)

instance (FunCxt cxt a, FunApp getb a ~ b)  => ApplyAB (Fun cxt getb) a b where
    applyAB (Fun f) x = f x

instance (FunCxt cxt b, FunApp geta b ~ a)  => ApplyAB (Fun' cxt geta) a b where
    applyAB (Fun' f) x = f x




-- ** Simple useful instances of Apply
-- | note this function will only be available at a single type
-- (that is, @hMap succ@ will only work on 'HList' that contain
-- only one type)
instance (x' ~ x, y' ~ y) => ApplyAB (x' -> y') x y where
  applyAB f x = f x



{- | print. An alternative implementation could be:

>>> let hPrint = Fun print :: Fun Show (IO ())

This produces:

>>> :t applyAB hPrint
applyAB hPrint :: Show a => a -> IO ()

-}
data HPrint = HPrint

instance (io ~ IO (), Show x) => ApplyAB HPrint x io where
  applyAB _ x = print x



{- | read

>>> applyAB HRead "5.0" :: Double
5.0

-}
data HRead = HRead
instance (String ~ string, Read a) => ApplyAB HRead string a where
    applyAB _ x = read x

-- | show
data HShow = HShow
instance (String ~ string, Show a) => ApplyAB HShow a string where
    applyAB _ x = show x





{- | Compose two instances of 'ApplyAB'

>>> applyAB (HComp HRead HShow) (5::Double) :: Double
5.0

-}
data HComp g f = HComp g f -- ^ @g . f@

instance forall f g a b c. (ApplyAB f a b, ApplyAB g b c) => ApplyAB (HComp g f) a c where
    applyAB ~(HComp g f) x = applyAB g (applyAB f x :: b)


{- | @app Comp (f,g) = g . f@. Works like:

>>> applyAB Comp (succ, pred) 'a'
'a'

>>> applyAB Comp (toEnum :: Int -> Char, fromEnum) 10
10

Note that defaulting will sometimes give you the wrong thing

> used to work (with associated types calculating result/argument types)
> >>> applyAB Comp (fromEnum, toEnum) 'a'
> *** Exception: Prelude.Enum.().toEnum: bad argument

-}
data Comp = Comp

instance (y ~ y', fg ~ (x -> y, y' -> z), r ~ (x -> z)) => ApplyAB Comp fg  r
 where
  applyAB _ (f,g) = g . f

-- | (\(a,b) -> f a >> b)
newtype HSeq x = HSeq x
instance (Monad m, ApplyAB f x fx, fx ~ m (), pair ~ (x,m ()),
          ApplyAB f x (m ()) ) => ApplyAB (HSeq f) pair fx where
  applyAB (HSeq f) (x,c) = do asVoid (applyAB f x); c
    where asVoid :: m () -> m ()
          asVoid t = t



-- | @HJust ()@ is a placeholder for a function that applies the 'HJust' constructor
instance hJustA ~ HJust a => ApplyAB (HJust t) a hJustA where
    applyAB _ a = HJust a


-- | 'flip'
data HFlip = HFlip

instance (f1 ~ (a -> b -> c), f2 ~ (b -> a -> c))  => ApplyAB HFlip f1 f2 where
    applyAB _ = flip


-- | 'fmap'
newtype HFmap f = HFmap f

instance (x ~ t a,
          y ~ t b,
          Functor t,
          ApplyAB f a b) =>
  ApplyAB (HFmap f) x y where
    applyAB (HFmap f) = fmap (applyAB f)


-- | 'liftA2'
newtype LiftA2 f = LiftA2 f

instance (ApplyAB f (x,y) z,
          mz ~ m z,
          mxy ~ (m x, m y),
          Applicative m) => ApplyAB (LiftA2 f) mxy mz where
    applyAB (LiftA2 f) xy = liftA2 (curry (applyAB f)) `uncurry` xy


-- | 'untag'
data HUntag = HUntag
instance (Tagged t x ~ tx) => ApplyAB HUntag tx x where
    applyAB _ (Tagged x) = x


-- --------------------------------------------------------------------------
-- * Proxy
--

-- $note see "Data.Proxy"

-- | A special 'Proxy' for record labels, polykinded
data Label l = Label

labelToProxy :: Label l -> Proxy l
labelToProxy _ = Proxy

class ShowLabel l where
  showLabel :: Label l -> String


-- --------------------------------------------------------------------------

-- * Booleans

{- $boolNote

GHC already lifts booleans, defined as

> data Bool = True | False

to types: Bool becomes kind and True and False (also denoted by
'True and 'False) become nullary type constructors.

The above line is equivalent to

> data HTrue
> data HFalse

> class HBool x
> instance HBool HTrue
> instance HBool HFalse

-}

-- ** Value-level proxies
hTrue  :: Proxy True ; hTrue  = Proxy
hFalse :: Proxy False; hFalse = Proxy


-- **  Conjunction

type family HAnd (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HAnd False t  = False
type instance HAnd True  t  = t

-- | `demote' to values
hAnd :: Proxy t1 -> Proxy t2 -> Proxy (HAnd t1 t2)
hAnd _ _ = Proxy


-- ** Disjunction

type family HOr (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HOr False t    = t
type instance HOr True t     = True

-- | `demote' to values
hOr :: Proxy t1 -> Proxy t2 -> Proxy (HOr t1 t2)
hOr _ _ = Proxy

{- $boolHistoricalNote

Compare with the original code based on functional dependencies:

> class (HBool t, HBool t', HBool t'') => HOr t t' t'' | t t' -> t''
>  where
>   hOr :: t -> t' -> t''

> instance HOr HFalse HFalse HFalse
>  where
>   hOr _ _ = hFalse

> instance HOr HTrue HFalse HTrue
>  where
>   hOr _ _ = hTrue

> instance HOr HFalse HTrue HTrue
>  where
>   hOr _ _ = hTrue

> instance HOr HTrue HTrue HTrue
>  where
>   hOr _ _ = hTrue
-}


class HCond (t :: Bool) x y z | t x y -> z
 where
  hCond :: Proxy t -> x -> y -> z

instance HCond False x y y
 where
  hCond _ _ y = y

instance HCond True x y x
 where
  hCond _ x _ = x


-- ** Boolean equivalence

type family HBoolEQ (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HBoolEQ False False    = True
type instance HBoolEQ False True     = False
type instance HBoolEQ True  False    = False
type instance HBoolEQ True  True     = True

-- We could define all kinds of further Boolean operations.
-- We omit everything what's not needed for the code in the paper.

-- --------------------------------------------------------------------------

-- * Naturals

-- | The data type to be lifted to the type level
data HNat = HZero | HSucc HNat


hZero :: Proxy HZero; hZero = Proxy
hSucc :: Proxy (n :: HNat) -> Proxy (HSucc n); hSucc _ = Proxy
hPred :: Proxy (HSucc n) -> Proxy n; hPred _ = Proxy

class HNat2Integral (n::HNat) where
    hNat2Integral :: Integral i => Proxy n -> i

instance HNat2Integral HZero where
    hNat2Integral _ = 0

instance HNat2Integral n => HNat2Integral (HSucc n) where
    hNat2Integral n = hNat2Integral (hPred n) + 1

instance HNat2Integral n => Show (Proxy (n :: HNat)) where
    show n = "H" ++ show (hNat2Integral n :: Integer)


-- | Equality on natural numbers
-- (eventually to be subsumed by the universal polykinded HEq)
type family HNatEq (t1 :: HNat) (t2 :: HNat) :: Bool
type instance HNatEq HZero HZero          = True
type instance HNatEq HZero (HSucc n)      = False
type instance HNatEq (HSucc n) HZero      = False
type instance HNatEq (HSucc n) (HSucc n') = HNatEq  n n'


-- | Less than

type family HLt (x :: HNat) (y :: HNat) :: Bool

type instance HLt HZero HZero          = False
type instance HLt HZero (HSucc n)      = True
type instance HLt (HSucc n) HZero      = False
type instance HLt (HSucc n) (HSucc n') = HLt  n n'

hLt :: Proxy x -> Proxy y -> Proxy (HLt x y)
hLt _ _ = Proxy


-- --------------------------------------------------------------------------
-- * Maybies
-- $maybiesNote We cannot use lifted Maybe since the latter are not populated

data    HNothing  = HNothing  deriving Show
newtype HJust x   = HJust x   deriving Show


-- --------------------------------------------------------------------------

-- * Polykinded Equality for types
-- | We have to use Functional dependencies for now,
-- for the sake of the generic equality.
class HEq (x :: k) (y :: k) (b :: Bool) | x y -> b

#if NEW_TYPE_EQ
-- unclear why this
instance ((Proxy x == Proxy y) ~ b) => HEq x y b
-- works better the version using (==) :: k -> k -> Bool
-- instance ((x == y) ~ b) => HEq x y b
#endif

hEq :: HEq x y b => x -> y -> Proxy b
hEq _ _ = Proxy


-- --------------------------------------------------------------------------

-- * Staged equality
-- |
--
--  * Establish type equality statically
--
--  * Establish remaining value-level equality dynamically
--
-- removed: use typeable


-- --------------------------------------------------------------------------
-- * Type-safe cast -- no longer need. We use a a ~ b

-- --------------------------------------------------------------------------

-- * Error messages

-- | A class without instances for explicit failure
class Fail x


-- ** Uses of fail
-- $note these could be replaced by `'("helpful message", l)`,
-- but these look better.
data ExtraField l = ExtraField
data FieldNotFound l = FieldNotFound



