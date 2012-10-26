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


-- Injection from algebraic kinds to *
-- Algebraic kinds like Nat are not populated and we can't use 
-- values of type Nat as function arguments. In contrast, we can use
-- (undefined :: Proxy Z) as an argument, as a value proxy.
-- data Proxy (tp :: k) :: *
data Proxy tp :: *

proxy :: Proxy tp
proxy =  undefined

-- --------------------------------------------------------------------------

-- * Booleans

-- GHC already lifts booleans, defined as
-- data Bool = True | False
-- to types: Bool becomes kind and True and False (also denoted by
-- 'True and 'False) become nullary type constructors.

-- The above line is equivalent to 
{-
data HTrue
data HFalse

class HBool x
instance HBool HTrue
instance HBool HFalse
-}

-- Value-level proxies
hTrue  :: Proxy True ; hTrue  = undefined
hFalse :: Proxy False; hFalse = undefined

instance Show (Proxy True)  where show _ = "HTrue"
instance Show (Proxy False) where show _ = "HFalse"


-- **  Conjunction

type family HAnd (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HAnd False t  = False
type instance HAnd True  t  = t

-- `demote' to values
hAnd :: Proxy t1 -> Proxy t2 -> Proxy (HAnd t1 t2)
hAnd = undefined


-- ** Disjunction

type family HOr (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HOr False t    = t
type instance HOr True t     = True

-- `demote' to values
hOr :: Proxy t1 -> Proxy t2 -> Proxy (HOr t1 t2)
hOr = undefined

-- Compare with the original code based on functional dependencies:
{-
class (HBool t, HBool t', HBool t'') => HOr t t' t'' | t t' -> t''
 where
  hOr :: t -> t' -> t''

instance HOr HFalse HFalse HFalse
 where
  hOr _ _ = hFalse

instance HOr HTrue HFalse HTrue
 where
  hOr _ _ = hTrue

instance HOr HFalse HTrue HTrue
 where
  hOr _ _ = hTrue

instance HOr HTrue HTrue HTrue
 where
  hOr _ _ = hTrue
-}

-- ** Boolean equivalence

type family HBoolEQ (t1 :: Bool) (t2 :: Bool) :: Bool
type instance HBoolEQ False False    = True
type instance HBoolEQ False True     = False
type instance HBoolEQ True  False    = False
type instance HBoolEQ True  True     = True


-- ** Conditional

class HCond (t :: Bool) x y where
    type HCondR t x y :: *
    hCond :: Proxy t -> x -> y -> HCondR t x y

instance HCond False x y where
    type HCondR False x y = y
    hCond _ x y = y

instance HCond True x y where
    type HCondR True x y = x
    hCond _ x y = x


-- We could define all kinds of further Boolean operations.
-- We omit everything what's not needed for the code in the paper.

-- --------------------------------------------------------------------------

-- * Naturals

-- The data type to be lifted to the type level
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


-- Equality on natural numbers

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
-- We cannot use lifted Maybe since the latter are not populated

data    HNothing  = HNothing  deriving Show
newtype HJust x   = HJust x   deriving Show


-- --------------------------------------------------------------------------

-- * Polykinded Equality for types

class HEq x y (b :: Bool) | x y -> b

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


-- --------------------------------------------------------------------------

-- * Type-safe cast -- no longer need. We use a a ~ b

{-
class TypeCast x y | x -> y, y -> x
 where
  typeCast :: x -> y
-}


-- --------------------------------------------------------------------------

-- * A phantom type for type proxies

data Proxy e
instance Show (Proxy e) where show _ = "Proxy"

toProxy :: e -> Proxy e
toProxy _ = undefined

unProxy :: Proxy e -> e
unProxy =  undefined


-- --------------------------------------------------------------------------

-- * Subtyping

class SubType l l'

subType :: SubType l l' => l -> l' -> ()
subType _ _ = ()


-- --------------------------------------------------------------------------

-- * Error messages

-- | A class without instances for explicit failure
class Fail x


-}
