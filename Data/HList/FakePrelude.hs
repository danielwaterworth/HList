{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, 
  FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.
-}

module Data.HList.FakePrelude where


-- --------------------------------------------------------------------------
-- * A heterogeneous apply operator

-- | simpler/weaker version where type information only propagates forward
-- with this one. 'app' defined below, is more complicated / verbose to define,
-- but it offers better type inference. Most uses have been converted to
-- 'app', so there is not much that can be done with 'Apply'.
class Apply f a where
  type ApplyR f a :: *
  apply :: f -> a -> ApplyR f a
  apply = undefined                     -- In case we use Apply for
                                        -- type-level computations only

{- $note

 Polymorphic functions are not first-class in haskell. One solution is to
 write an instance of 'ApplyAB' for a data type that takes the place of
 the original function. In other words,

 > data Fn = Fn
 > instance ApplyAB Fn a b where applyAB Fn a = actual_fn a

 Normally you would have been able to pass around the definition actual_fn.

 [@Type inference@]

 The definiton of 'ApplyAB' requires all three types (function @f@, argument
 @a@ and result @b@) to be known before picking an implementation. This is
 impractical, since it means that every usage of 'applyAB' will need a type
 signature which is a pain.

 Specifying that the types should be restricted is accomplished by the use of
 two type families (described here since haddock does a bad job of them). These
 are (type-level) functions which compute (given @f@ @a@ @b@ from
 an instance of @ApplyAB@):

 > ApplyB f a = Maybe b
 > ApplyA f b = Maybe a

-}

-- | No constraints on result and argument types
class ApplyAB f a b where

  -- | @GetApplyB f a = b@ when it can be calculated
  type ApplyB f a :: Maybe k

  -- | @GetApplyA f b = a@ when it can be calculated
  type ApplyA f b :: Maybe k

  applyAB :: f -> a -> b
  applyAB = undefined -- In case we use Apply for type-level computations only


-- * working with promoted Maybe
type family GuardJust (a :: Maybe *) b :: *
type instance GuardJust Nothing b = ()
type instance GuardJust (Just a) b = b

type family FromMaybe b (a :: Maybe *) :: *
type instance FromMaybe b Nothing  = b
type instance FromMaybe b (Just a) = a

type family FromJust (a :: Maybe *) :: *
type instance FromJust (Just a) = a


{- | a single name for (the same behavior as)

> class Apply f a b | f a -> b, f b -> a where apply :: f -> a -> b
> class Apply f a b | f a -> b           where apply :: f -> a -> b
> class Apply f a b |           f b -> a where apply :: f -> a -> b

The fundeps are present if the associated types 'ApplyA' and 'ApplyB'
produce types that are 'Just', or absent if they are 'Nothing'.

-}
type App f a b = (ApplyAB f a b,
    GuardJust (ApplyA f b) a ~ FromMaybe () (ApplyA f b) ,
    GuardJust (ApplyB f a) b ~ FromMaybe () (ApplyB f a) )

app :: App f a b => f -> a -> b
app = applyAB

-- ** more restrictive apply
{- $example

Not clear if these really work properly yet.

Using standard \'standard\' functions read, show and Just,
only the following restricted versions of apply are accepted

>>> :t applyB' HRead
applyB' HRead :: Read b => [Char] -> b

>>> :t applyA' HShow
applyA' HShow :: Show a => a -> [Char]


>>> :t applyA' (HJust ())
applyA' (HJust ()) :: a -> HJust a

> ??
> >>> :t applyB' (HJust ())
> applyB' (HJust ()) :: a -> HJust a

>>> :t apply' (HJust ())
apply' (HJust ()) :: a -> HJust a

The other combinations should not typecheck (not tested here...)

-}

-- | must have a @| f b -> a@
type ApplyB' f a b = (ApplyAB f a b, a ~ FromJust (ApplyA f b) )

-- | must have a @| f a -> b@
type ApplyA' f a b = (ApplyAB f a b, b ~ FromJust (ApplyB f a) )

-- | must have a @| f a -> b, f b -> a@
type Apply' f a b = (ApplyAB f a b, b ~ FromJust (ApplyB f a), a ~ FromJust (ApplyA f b) )


apply' :: Apply' f a b => f -> a -> b
apply' = applyAB

applyA' :: ApplyA' f a b => f -> a -> b
applyA' = applyAB

applyB' :: ApplyB' f a b => f -> a -> b
applyB' = applyAB

-- ** Simple useful instances of Apply
instance ApplyAB (x -> y) x y where
  type ApplyB (x -> y) x = Just y
  type ApplyA (x -> y) y = Just x
  applyAB f x = f x



-- | print
data HPrint = HPrint

instance Show x => ApplyAB HPrint x (IO ()) where
  type ApplyB HPrint x = Just (IO ())
  type ApplyA HPrint (IO ()) = Nothing
  applyAB _ x = print x


{- | read

>>> app HRead "5.0" :: Double
5.0

-}
data HRead = HRead
instance Read a => ApplyAB HRead String a where
    type ApplyA HRead a = Just String
    type ApplyB HRead String = Nothing
    applyAB _ x = read x

-- | show
data HShow = HShow
instance Show a => ApplyAB HShow a String where
    type ApplyA HShow String = Nothing
    type ApplyB HShow a = Just String
    applyAB _ x = show x


{- | Compose two instances of 'ApplyAB'

>>> app (HComp HRead HShow) (5::Double) :: Double
5.0

-}
data HComp g f = HComp g f -- ^ @g . f@

type family JoinMaybe (a :: Maybe (Maybe k)) :: Maybe k
type instance JoinMaybe (Just a) = a
type instance JoinMaybe Nothing = Nothing

type family BindApplyA f (a :: Maybe *) :: Maybe *
type instance BindApplyA f Nothing = Nothing
type instance BindApplyA f (Just a) = ApplyA f a

type family BindApplyB f (a :: Maybe *) :: Maybe *
type instance BindApplyB f Nothing = Nothing
type instance BindApplyB f (Just a) = ApplyB f a

instance forall f g a b c. (App f a b, App g b c) => ApplyAB (HComp g f) a c where
    type ApplyA (HComp g f) c = BindApplyA f (ApplyA g c)
    type ApplyB (HComp g f) a = BindApplyB g (ApplyB f a)
    applyAB ~(HComp g f) x = app g (app f x :: b)


{- | @app Comp (f,g) = g . f@. Works like:

>>> app Comp (succ, pred) 'a'
'a'

>>> app Comp (toEnum :: Int -> Char, fromEnum) 10
10

Note that defaulting will sometimes give you the wrong thing

>>> app Comp (fromEnum, toEnum) 'a'
*** Exception: Prelude.Enum.().toEnum: bad argument

-}
data Comp = Comp

instance y ~ y' => ApplyAB Comp (x -> y,y' -> z) (x -> z)
 where
  type ApplyB Comp (x -> y,y' -> z) = Just (x -> z)
  type ApplyA Comp (x -> z)  = Nothing
  applyAB _ (f,g) = g . f

-- | (\(a,b) -> f a >> b)
newtype HSeq x = HSeq x
instance (Monad m, ApplyB f x ~ Just (m ()),
          App f x (m ()) ) => ApplyAB (HSeq f) (x,m ()) (m ()) where
  type ApplyA (HSeq f) (m ()) = Nothing
  type ApplyB (HSeq f) (x,m ()) = Just (m ())
  applyAB (HSeq f) (x,c) = do app f x; c



-- | @HJust ()@ is a placeholder for a function that applies the 'HJust' constructor
instance ApplyAB (HJust ()) a (HJust a) where
    type ApplyA (HJust ()) (HJust a) = Just a
    type ApplyB (HJust ()) a = Just (HJust a)
    applyAB _ a = HJust a


-- | 'flip'
data HFlip = HFlip

instance ApplyAB HFlip (a -> b -> c) (b -> a -> c) where
    type ApplyB HFlip (a -> b -> c) = Just (b -> a -> c)
    type ApplyA HFlip (b -> a -> c) = Just (a -> b -> c)
    applyAB _ = flip

-- --------------------------------------------------------------------------
-- * Proxy
--
-- | Injection from algebraic kinds to *
-- Algebraic kinds like Nat are not populated and we can't use 
-- values of type Nat as function arguments. In contrast, we can use
-- (undefined :: Proxy Z) as an argument, as a value proxy.
-- data Proxy (tp :: k) :: *
data Proxy tp 

proxy :: Proxy tp
proxy =  undefined

-- | A special 'Proxy' for record labels, polykinded
data Label l = Label

labelToProxy :: Label l -> Proxy l
labelToProxy = undefined


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
hTrue  :: Proxy True ; hTrue  = undefined
hFalse :: Proxy False; hFalse = undefined

instance Show (Proxy True)  where show _ = "HTrue"
instance Show (Proxy False) where show _ = "HFalse"


-- **  Conjunction

type family HAnd (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HAnd False t  = False
type instance HAnd True  t  = t

-- | `demote' to values
hAnd :: Proxy t1 -> Proxy t2 -> Proxy (HAnd t1 t2)
hAnd = undefined


-- ** Disjunction

type family HOr (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HOr False t    = t
type instance HOr True t     = True

-- | `demote' to values
hOr :: Proxy t1 -> Proxy t2 -> Proxy (HOr t1 t2)
hOr = undefined

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


hZero :: Proxy HZero; hZero = undefined
hSucc :: Proxy (n :: HNat) -> Proxy (HSucc n); hSucc _ = undefined
hPred :: Proxy (HSucc n) -> Proxy n; hPred _ = undefined

class HNat2Integral (n::HNat) where
    hNat2Integral :: Integral i => Proxy n -> i

instance HNat2Integral HZero where
    hNat2Integral _ = 0

instance HNat2Integral n => HNat2Integral (HSucc n) where
    hNat2Integral n = hNat2Integral (hPred n) + 1

instance HNat2Integral n => Show (Proxy (n :: HNat)) where 
    show n = "H" ++ show (hNat2Integral n)


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
hLt = undefined


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

-- Equality instances for naturals

instance HEq HZero HZero     True
instance HEq HZero (HSucc n) False
instance HEq (HSucc n) HZero False
instance HEq  n n' b => HEq (HSucc n) (HSucc n') b

hEq :: HEq x y b => x -> y -> Proxy b
hEq =  undefined

{-

-- --------------------------------------------------------------------------

-- * Staged equality
-- |
--
--  * Establish type equality statically
--
--  * Establish remaining value-level equality dynamically

class HStagedEq x y
 where
  hStagedEq :: x -> y -> Bool


-- --------------------------------------------------------------------------

-- | A predicate for type equality
--
-- There are different implementations: see TypeEq*.hs

class HBool b => TypeEq x y b | x y -> b


-- Rely on lazy show for type-level Booleans
typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined


-- A more disciplined version: based on proxies
proxyEq :: TypeEq t t' b => Proxy t -> Proxy t' -> b
proxyEq _ _ = undefined

-}

-- --------------------------------------------------------------------------
-- * Type-safe cast -- no longer need. We use a a ~ b

{-
class TypeCast x y | x -> y, y -> x
 where
  typeCast :: x -> y
-}


-- --------------------------------------------------------------------------

-- * Error messages

-- | A class without instances for explicit failure
class Fail x

