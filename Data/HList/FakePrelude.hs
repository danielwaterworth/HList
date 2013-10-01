
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.
-}

module Data.HList.FakePrelude where

import GHC.Prim (Constraint)
import GHC.TypeLits

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
 @ApplyAB Fn Int Double@. Additional explanation can be found
 in <http://okmij.org/ftp/Haskell/typecast.html#local-fd local functional dependencies>

-}

-- | No constraints on result and argument types
class ApplyAB f a b where
  applyAB :: f -> a -> b
  applyAB = undefined -- In case we use Apply for type-level computations only


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
 applyAB plus1 :: (Num a, Ord a) => a -> a

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


