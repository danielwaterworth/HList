{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.

-}

  
module FakePrelude where


{-----------------------------------------------------------------------------}

-- Type-level Booleans

data HTrue  = HTrue
data HFalse = HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


class HBool x
instance HBool HTrue
instance HBool HFalse


-- Disjunction of type-level Booleans

class (HBool t, HBool t', HBool t'') => HOr t t' t'' | t t' -> t''
 where
  hOr :: t -> t' -> t''

instance HOr HFalse HFalse HFalse
 where
  hOr _ _ = HFalse

instance HOr HTrue HFalse HTrue
 where
  hOr _ _ = HTrue

instance HOr HFalse HTrue HTrue
 where
  hOr _ _ = HTrue

instance HOr HTrue HTrue HTrue
 where
  hOr _ _ = HTrue


-- Type-level conditional

class HBool t => HCond t x y z | t x y -> z
 where
  hCond :: t -> x -> y -> z
 
instance HCond HFalse x y y
 where
  hCond _ _ y = y
 
instance HCond HTrue x y x
 where
  hCond _ x _ = x


-- We could define all kinds of further Boolean operations.
-- We omit everything what's not needed for the code in the paper.


{-----------------------------------------------------------------------------}

-- Type-level naturals

data HZero   = HZero    deriving Show
data HSucc n = HSucc n  deriving Show


-- Access predecessor of a type
-- This allows us to avoid references to data constructors for HNat.
--
hPrec :: HSucc n -> n
hPrec =  undefined


-- The value-level reification is for convenience only.
-- We can operate at the type level normally.
--
class HNat n
 where
  hNat2Integral :: Integral i => n -> i
 
instance HNat HZero
 where
  hNat2Integral _ = 0
 
instance HNat n => HNat (HSucc n)
 where
  hNat2Integral (HSucc n) = hNat2Integral n + 1


{-----------------------------------------------------------------------------}

-- Type-level maybies

data HNothing  = HNothing  deriving Show
data HJust x   = HJust x   deriving Show


{-----------------------------------------------------------------------------}

-- Equality for types

class HBool b => HEq x y b | x y -> b
 where
  hEq :: x -> y -> b


-- Equality instances for naturals

instance HEq HZero HZero HTrue
 where
  hEq _ _ = HTrue

instance HNat n => HEq HZero (HSucc n) HFalse
 where
  hEq _ _ = HFalse

instance HNat n => HEq (HSucc n) HZero HFalse
 where
  hEq _ _ = HFalse

instance ( HNat n
         , HNat n'
         , HEq  n n' b 
         )
      =>   HEq (HSucc n) (HSucc n') b
 where
  hEq n n' = hEq (hPrec n) (hPrec n')


{-----------------------------------------------------------------------------}

-- Less than

class HBool b => HLt x y b | x y -> b
 where
  hLt :: x -> y -> b


-- Equality instances for naturals

instance HLt HZero HZero HFalse
 where
  hLt _ _ = HFalse

instance HNat n => HLt HZero (HSucc n) HTrue
 where
  hLt _ _ = HTrue

instance HNat n => HLt (HSucc n) HZero HFalse
 where
  hLt _ _ = HFalse

instance ( HNat n
         , HNat n'
         , HLt  n n' b 
         )
      =>   HLt (HSucc n) (HSucc n') b
 where
  hLt n n' = hLt (hPrec n) (hPrec n')


{-----------------------------------------------------------------------------}

-- A predicate for type equality
-- There are different implementations.
-- See imports in Main*.hs

class TypeEqBool x y b | x y -> b


-- Rely on lazy show for type-level Booleans
typeEqBool :: TypeEqBool t t' b => t -> t' -> b
typeEqBool = undefined


-- A more disciplined version: based on proxies
proxyEqBool :: TypeEqBool t t' b => HProxy t -> HProxy t' -> b
proxyEqBool x y = typeEqBool (hUnProxy x) (hUnProxy y)


{-----------------------------------------------------------------------------}

-- Type-safe cast

class Cast x y | x -> y
 where
  cast :: x -> y


{-----------------------------------------------------------------------------}
 
-- A phantom type for type proxies
 
data HProxy e = HProxy deriving Show

hProxy   :: e -> HProxy e
hProxy _ =  HProxy

hUnProxy :: HProxy e -> e
hUnProxy =  undefined


{-----------------------------------------------------------------------------}

-- Type equality and disequality

class TypeEq    x y
class TypeNotEq x y

typeEq :: TypeEq x y => x -> y -> ()
typeEq _ _ = ()

typeNotEq :: TypeNotEq x y => x -> y -> ()
typeNotEq _ _ = ()


{-----------------------------------------------------------------------------}
 
-- A class without instances for explicit failure
class Fail z


{-----------------------------------------------------------------------------}
