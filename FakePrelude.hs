{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.

-}

  
module FakePrelude where


{-----------------------------------------------------------------------------}

-- Type-level Booleans

data HTrue; hTrue :: HTrue; hTrue = undefined
data HFalse; hFalse :: HFalse; hFalse = undefined
class HBool x; instance HBool HTrue; instance HBool HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


-- Conjunction of type-level Booleans

class (HBool t, HBool t', HBool t'') => HAnd t t' t'' | t t' -> t''
 where
  hAnd :: t -> t' -> t''

instance HAnd HFalse HFalse HFalse
 where
  hAnd _ _ = hFalse

instance HAnd HTrue HFalse HFalse
 where
  hAnd _ _ = hFalse

instance HAnd HFalse HTrue HFalse
 where
  hAnd _ _ = hFalse

instance HAnd HTrue HTrue HTrue
 where
  hAnd _ _ = hTrue


-- Disjunction of type-level Booleans

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
hPred :: HSucc n -> n
hPred =  undefined


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
  hEq _ _ = hTrue

instance HNat n => HEq HZero (HSucc n) HFalse
 where
  hEq _ _ = hFalse

instance HNat n => HEq (HSucc n) HZero HFalse
 where
  hEq _ _ = hFalse

instance ( HNat n
         , HNat n'
         , HEq  n n' b 
         )
      =>   HEq (HSucc n) (HSucc n') b
 where
  hEq n n' = hEq (hPred n) (hPred n')


{-----------------------------------------------------------------------------}

-- Staged equality
--  - Establish type equality statically
--  - Establish remaining value-level equality dynamically

class HStagedEq x y
 where
  hStagedEq :: x -> y -> Bool
 

{-----------------------------------------------------------------------------}

-- Less than

class HBool b => HLt x y b | x y -> b
 where
  hLt :: x -> y -> b


-- Equality instances for naturals

instance HLt HZero HZero HFalse
 where
  hLt _ _ = hFalse

instance HNat n => HLt HZero (HSucc n) HTrue
 where
  hLt _ _ = hTrue

instance HNat n => HLt (HSucc n) HZero HFalse
 where
  hLt _ _ = hFalse

instance ( HNat n
         , HNat n'
         , HLt  n n' b 
         )
      =>   HLt (HSucc n) (HSucc n') b
 where
  hLt n n' = hLt (hPred n) (hPred n')


{-----------------------------------------------------------------------------}

-- A predicate for type equality
-- There are different implementations.
-- See imports in Main*.hs

class HBool b => TypeEq x y b | x y -> b


-- Rely on lazy show for type-level Booleans
typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined


-- A more disciplined version: based on proxies
proxyEq :: TypeEq t t' b => Proxy t -> Proxy t' -> b
proxyEq x y = undefined


{-----------------------------------------------------------------------------}

-- Type-safe cast

class TypeCast x y | x -> y, y -> x
 where
  typeCast :: x -> y


{-----------------------------------------------------------------------------}
 
-- A phantom type for type proxies
 
data Proxy e = Proxy deriving Show

proxy   :: e -> Proxy e
proxy _ =  Proxy

unProxy :: Proxy e -> e
unProxy =  undefined


{-----------------------------------------------------------------------------}

-- Type equality and disequality

class TypeEqTrue x y
class TypeEqFalse x y

typeEqTrue :: TypeEqTrue x y => x -> y -> ()
typeEqTrue _ _ = ()

typeEqFalse :: TypeEqFalse x y => x -> y -> ()
typeEqFalse _ _ = ()


{-----------------------------------------------------------------------------}

-- Subtyping

class SubType l l'

subType :: SubType l l' => l -> l' -> ()
subType _ _ = ()


{-----------------------------------------------------------------------------}
 
-- A class without instances for explicit failure
class Fail x


{-----------------------------------------------------------------------------}
