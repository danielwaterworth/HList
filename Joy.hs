{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

--Joy implemented in Haskell... extensible embedded language...

module Joy where

import MainGhcGeneric1

-- Building non-empty lists

type HOne = HSucc HZero
hOne :: HOne
hOne = undefined
type HTwo = HSucc HOne
hTwo :: HTwo
hTwo = undefined
type HThree = HSucc HTwo
hThree :: HThree
hThree = undefined

end :: HNil
end = hNil

instance HList s => Apply HNil s s where
	apply _ s = s
instance (HList s,HList s',HList l,Apply a s s',Apply l s' s'') => Apply (HCons a l) s s'' where
	apply (HCons a l) s = apply l (apply a s :: s')
instance HList s => Apply HZero s (HCons HZero s) where
	apply _ s = hCons hZero s
instance (HNat a,HList s) => Apply (HSucc a) s (HCons (HSucc a) s) where
	apply a s = hCons a s

data Lit a = Lit a
lit :: a -> Lit a
lit a = Lit a
unl :: Lit a -> a
unl (Lit a) = a
instance Show a => Show (Lit a) where
	showsPrec _ (Lit a) = showChar '[' . shows a . showChar ']'
instance HList s => Apply (Lit a) s (HCons a s) where
	apply (Lit a) s = hCons a s

class (HBool b,HList s) => HIfte b t f s s' | b t f s -> s' where
	hIfte :: b -> t -> f -> s -> s'
instance (HList s,Apply t s s') => HIfte HTrue t f s s' where
	hIfte _ t _ s = apply t s
instance (HList s,Apply f s s') => HIfte HFalse t f s s' where
	hIfte _ _ f s = apply f s

data Ifte
ifte :: Ifte
ifte = undefined
instance Show Ifte where
	showsPrec _ _ = showString "If"
instance (Apply b s r,HHead r b',HIfte b' t f s s')
	=> Apply Ifte (f :*: t :*: b :*: s) s' where
	apply _ (HCons f (HCons t (HCons b s))) = hIfte (hHead (apply b s :: r) :: b') t f s

data Nul
nul :: Nul
nul = undefined
instance Show Nul where
	showsPrec _ _ = showString "Nul"
instance HList s => Apply Nul (HCons HZero s) (HCons HTrue s) where
	apply _ (HCons _ s) = hCons hTrue s
instance HList s => Apply Nul (HCons (HSucc n) s) (HCons HFalse s) where
	apply _ (HCons _ s) = hCons hFalse s

data EQ
eq :: EQ
eq = undefined
instance Show EQ where
	showsPrec _ _ = showString "Eq"
instance (HList s,TypeEq a b t) => Apply EQ (HCons a (HCons b s)) (HCons t s) where
	apply _ (HCons a (HCons b s)) = hCons (typeEq a b) s

data Dip
dip :: Dip
dip = undefined
instance Show Dip where
	showsPrec _ _ = showString "Dip"
instance (HList s,HList s',Apply a s s') => Apply Dip (HCons a (HCons b s)) (HCons b s') where
	apply _ (HCons a (HCons b s)) = hCons b (apply a s)

data Dup 
dup :: Dup
dup = undefined
instance Show Dup where
	showsPrec _ _ = showString "Dup"
instance HList s => Apply Dup (HCons a s) (HCons a (HCons a s)) where
	apply _ s@(HCons a _) = hCons a s

data Pop
pop :: Pop
pop = undefined
instance Show Pop where
	showsPrec _ _ = showString "Pop"
instance HList s => Apply Pop (HCons a s) s where
	apply _ (HCons _ s) = s

data Swap
swap :: Swap
swap = undefined
instance Show Swap where
	showsPrec _ _ = showString "Swap"
instance HList s => Apply Swap (HCons a (HCons b s)) (HCons b (HCons a s)) where
	apply _ (HCons a (HCons b s)) = hCons b (hCons a s)

data Suc
suc :: Suc
suc = undefined
instance Show Suc where
	showsPrec _ _ = showString "Suc"
instance (HNat a,HList s) => Apply Suc (HCons a s) (HCons (HSucc a) s) where
	apply _ (HCons _ s) = hCons (undefined::HSucc a) s

data Pre
pre :: Pre
pre = undefined
instance Show Pre where
	showsPrec _ _ = showString "Pre"
instance (HNat a,HList s) => Apply Pre (HCons (HSucc a) s) (HCons a s) where
	apply _ (HCons _ s) = hCons (undefined::a) s

data Add
add :: Add
add = undefined
instance Show Add where
	showsPrec _ _ = showString "Add"
instance (HList s,HAdd a b c) => Apply Add (HCons a (HCons b s)) (HCons c s) where
	apply _ (HCons _ (HCons _ s)) = hCons (hAdd (undefined::a) (undefined::b)) s

class (HNat a,HNat b) => HAdd a b c | a b -> c where
	hAdd :: a -> b -> c
instance HAdd HZero HZero HZero where
	hAdd _ _ = hZero
instance HNat b => HAdd HZero (HSucc b) (HSucc b) where
	hAdd _ b = b
instance HNat a => HAdd (HSucc a) HZero (HSucc a) where
	hAdd a _ = a
instance (HNat (HSucc a),HNat (HSucc b),HNat c,HAdd a b c)
	=> HAdd (HSucc a) (HSucc b) (HSucc (HSucc c)) where
	hAdd _ _ = hSucc $ hSucc $ hAdd (undefined::a) (undefined::b)

data Sub
sub :: Sub
sub = undefined
instance Show Sub where
	showsPrec _ _ = showString "Sub"
instance (HList s,HSub a b c) => Apply Sub (HCons b (HCons a s)) (HCons c s) where
	apply _ (HCons _ (HCons _ s)) = hCons (hSub (undefined::a) (undefined::b)) s

class (HNat a,HNat b) => HSub a b c | a b -> c where
	hSub :: a -> b -> c
instance HSub HZero HZero HZero where
	hSub _ _ = hZero
instance HNat a => HSub (HSucc a) HZero (HSucc a) where
	hSub a _ = a
instance HNat a => HSub HZero (HSucc a) HZero where
	hSub _ _ = hZero
instance (HSub a b c) => HSub (HSucc a) (HSucc b) c where
	hSub _ _ = hSub (undefined::a) (undefined::b)
	
data Mult
mult :: Mult
mult = undefined
instance Show Mult where
	showsPrec _ _ = showString "Mult"
instance (HList s,HMult a b c) => Apply Mult (HCons a (HCons b s)) (HCons c s) where
	apply _ (HCons _ (HCons _ s)) = hCons (hMult (undefined::a) (undefined::b)) s

class (HNat a,HNat b) => HMult a b c | a b -> c where
	hMult :: a -> b -> c
instance HNat b => HMult HZero b HZero where
	hMult _ _ = hZero
instance (HMult a b s,HAdd b s s') => HMult (HSucc a) b s' where
	hMult _ _ = hAdd (undefined::b) (hMult (undefined::a) (undefined::b) :: s)

square = dup .*. mult .*. hNil
cube = mult .*. mult .*. dup .*. dup .*. hNil

data I
i :: I
i = undefined
instance Show I where
	showsPrec _ _ = showString "I"
instance Apply I HNil HNil where
	apply _ _ = hNil
instance (HList s,Apply a s s') => Apply I (HCons a s) s' where
	apply _ (HCons a s) = apply a s

data Primrec = Primrec deriving Show
primrec :: Primrec
primrec = undefined
instance Apply z s s' => Apply Primrec (HCons nz (HCons z (HCons HZero s))) s' where
	apply _ (HCons _ (HCons z (HCons _ s))) = apply z s
instance (HList s,Apply Primrec (HCons nz (HCons z (HCons n (HCons (HSucc n) s)))) s',Apply nz s' s'')
	=> Apply Primrec (HCons nz (HCons z (HCons (HSucc n) s))) s'' where
	apply _ (HCons nz (HCons z s@(HCons _ _))) = apply nz (apply Primrec (hCons nz (hCons z (hCons (undefined::n) s))))

data Times
times :: Times
times = undefined
instance Show Times where
	showsPrec _ _ = showString "Times"
instance HList s => Apply Times (HCons p (HCons HZero s)) s where
	apply _ (HCons _ (HCons _ s)) = s
instance (HNat n,HList s,HList s',Apply p s s',Apply Times (HCons p (HCons n s')) s'')
	=> Apply Times (HCons p (HCons (HSucc n) s)) s'' where
	apply _ (HCons p (HCons _ s)) = apply times (hCons p (hCons (undefined::n) (apply p s)))

class (HBool f,HList s) => HGenrec f r1 r2 b t s s'' | f r1 r2 b t s -> s'' where
	hGenrec :: f -> r1 -> r2 -> b -> t -> s -> s''
instance (HList s,Apply t s s') => HGenrec HTrue r1 r2 b t s s' where
	hGenrec _ _ _ _ t s = apply t s
instance (HList s,HList s',Apply r1 s s',
	Apply (HCons (Lit (HCons (Lit b) (HCons (Lit t) (HCons (Lit r1) (HCons (Lit r2) (HCons Genrec HNil)))))) (HCons r2 HNil)) s' s'')
	=> HGenrec HFalse r1 r2 b t s s'' where
	hGenrec _ r1 r2 b t s = apply (hCons (lit (hCons (lit b) (hCons (lit t) (hCons (lit r1) (hCons (lit r2) (hCons genrec hNil)))))) (hCons r2 hNil)) (apply r1 s :: s') 

data Genrec
genrec :: Genrec
genrec = undefined
instance Show Genrec where
	showsPrec _ _ = showString "Genrec"
instance (Apply b s s',HHead s' b',HGenrec b' r1 r2 b t s s'')
	=> Apply Genrec (HCons r2 (HCons r1 (HCons t (HCons b s)))) s'' where
	apply _ (HCons r2 (HCons r1 (HCons t (HCons b s))))
		= hGenrec (hHead (apply b s :: s') :: b') r1 r2 b t s

class (HBool f,HList s) => HLinrec f b t r1 r2 s s' | f b t r1 r2 s -> s' where
	hLinrec :: f -> b -> t -> r1 -> r2 -> s -> s'
instance (HList s,Apply t s s') => HLinrec HTrue b t r1 r2 s s' where
	hLinrec _ _ t _ _ s = apply t s
instance (HList s,HList s',Apply r1 s s',
	Apply Linrec (HCons r2 (HCons r1 (HCons t (HCons b s')))) s'',Apply r2 s'' s''')
	=> HLinrec HFalse b t r1 r2 s s''' where
	hLinrec _ b t r1 r2 s = apply r2 (apply linrec (hCons r2 (hCons r1 (hCons t (hCons b (apply r1 s :: s'))))) :: s'')

data Linrec
linrec :: Linrec
linrec = undefined
instance Show Linrec where
	showsPrec _ _ = showString "Linrec"
instance (Apply b s s',HHead s' b',HLinrec b' b t r1 r2 s s'') => Apply Linrec (HCons r2 (HCons r1 (HCons t (HCons b s)))) s'' where
	apply _ (HCons r2 (HCons r1 (HCons t (HCons b s)))) = hLinrec (hHead (apply b s :: s') :: b') b t r1 r2 s

data Fact
fact :: Fact
fact = undefined
instance Show Fact where
	showsPrec _ _ = showString "Fact"
instance (HList s,Apply (HCons (Lit (HCons (Lit HZero) (HCons EQ HNil)))
	(HCons (Lit (HCons Pop (HCons (Lit HOne) HNil)))
	(HCons (Lit (HCons Dup
	(HCons (Lit HOne)
	(HCons Sub (HCons Fact (HCons Mult HNil))))))
	(HCons Ifte HNil)))) s s') => Apply Fact s s' where
	apply _ s = apply fac1 s

fac1 = hCons (lit (hCons (lit hZero) (hCons eq hNil)))
	(hCons (lit (hCons pop (hCons (lit hOne) hNil)))
	(hCons (lit (hCons dup (hCons (lit hOne) (hCons sub (hCons fact (hCons mult hNil))))))
	(hCons ifte hNil)))

fac2 = lit (hOne .*. hOne .*. end)
	.*. dip .*. lit (dup .*. lit mult .*. dip .*. suc .*. end)
	.*. times .*. pop .*. end

fac3 = lit nul .*. lit suc .*. lit (dup .*. pre .*. end)
	.*. lit (i .*. mult .*. end) .*. genrec .*. end

fac4 = lit nul .*. lit suc .*. lit (dup .*. pre .*. end)
	.*. lit mult .*. linrec .*. end

fac5 = lit hOne .*. lit mult .*. primrec .*. end

main :: IO ()
main = do
	putStrLn $ show $ apply (lit hThree .*. fac1 .*. end) end
	putStrLn $ show $ apply i (fac2 .*. hThree .*. end)
	putStrLn $ show $ apply i (fac3 .*. hThree .*. end)
	putStrLn $ show $ apply i (fac4 .*. hThree .*. end)
	putStrLn $ show $ apply i (fac5 .*. hThree .*. end)

