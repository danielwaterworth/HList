{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

--Joy implemented in Haskell... extensible embedded language...

module Joy where

import MainGhcGeneric1

type HOne = HSucc HZero
hOne :: HOne
hOne = undefined
type HTwo = HSucc HOne
hTwo :: HTwo
hTwo = undefined
type HThree = HSucc HTwo
hThree :: HThree
hThree = undefined

data Lit a
lit :: a -> Lit a
lit = undefined
unl :: Lit a -> a
unl = undefined
instance HList s => Apply (Lit a) s (HCons a s) where
	apply a s = hCons (unl a) s

class (HBool b,HList s) => HIfte b t f s s' | b t f s -> s' where
	hIfte :: b -> t -> f -> s -> s'
instance (HList s,Apply t s s') => HIfte HTrue t f s s' where
	hIfte _ t _ s = apply t s
instance (HList s,Apply f s s') => HIfte HFalse t f s s' where
	hIfte _ _ f s = apply f s

data Ifte
ifte :: Ifte
ifte = undefined
instance (Apply b s r,HHead r b',HIfte b' t f s s')
	=> Apply Ifte (HCons f (HCons t (HCons b s))) s' where
	apply _ (HCons f (HCons t (HCons b s))) = hIfte (hHead (apply b s :: r) :: b') t f s

data Nul
nul :: Nul
nul = undefined
instance HList s => Apply Nul (HCons HZero s) (HCons HTrue s) where
	apply _ (HCons _ s) = hCons hTrue s
instance HList s => Apply Nul (HCons (HSucc n) s) (HCons HFalse s) where
	apply _ (HCons _ s) = hCons hFalse s

data EQ
eq :: EQ
eq = undefined
instance (HList s,TypeEq a b t) => Apply EQ (HCons a (HCons b s)) (HCons t s) where
	apply _ (HCons a (HCons b s)) = hCons (typeEq a b) s

data Dip
dip :: Dip
dip = undefined
instance (HList s,HList s',Apply a s s') => Apply Dip (HCons a (HCons b s)) (HCons b s') where
	apply _ (HCons a (HCons b s)) = hCons b (apply a s)

data Dup
dup :: Dup
dup = undefined
instance HList s => Apply Dup (HCons a s) (HCons a (HCons a s)) where
	apply _ s@(HCons a _) = hCons a s

data Pop
pop :: Pop
pop = undefined
instance HList s => Apply Pop (HCons a s) s where
	apply _ (HCons _ s) = s

data Swap
swap :: Swap
swap = undefined
instance HList s => Apply Swap (HCons a (HCons b s)) (HCons b (HCons a s)) where
	apply _ (HCons a (HCons b s)) = hCons b (hCons a s)

data Suc
suc :: Suc
suc = undefined
instance (HNat a,HList s) => Apply Suc (HCons a s) (HCons (HSucc a) s) where
	apply _ (HCons _ s) = hCons (undefined::HSucc a) s

data Pre
pre :: Pre
pre = undefined
instance (HNat a,HList s) => Apply Pre (HCons (HSucc a) s) (HCons a s) where
	apply _ (HCons _ s) = hCons (undefined::a) s

data Add
add :: Add
add = undefined
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
instance (HList s,HMult a b c) => Apply Mult (HCons a (HCons b s)) (HCons c s) where
	apply _ (HCons _ (HCons _ s)) = hCons (hMult (undefined::a) (undefined::b)) s

class (HNat a,HNat b) => HMult a b c | a b -> c where
	hMult :: a -> b -> c
instance HNat b => HMult HZero b HZero where
	hMult _ _ = hZero
instance (HMult a b s,HAdd b s s') => HMult (HSucc a) b s' where
	hMult _ _ = hAdd (undefined::b) (hMult (undefined::a) (undefined::b) :: s)

data Seq a b
o :: a -> b -> (Seq a b)
o _ _ = undefined
leftOp :: (Seq a b) -> a
leftOp _ = undefined
rightOp :: (Seq a b) -> b
rightOp _ = undefined
instance (HList s,Apply a s s',Apply b s' s'') => Apply (Seq a b) s s'' where
	apply op s = apply (rightOp op) (apply (leftOp op) s :: s')

square = dup `o` mult
cube = dup `o` dup `o` mult `o` mult

data I
i :: I
i = undefined
instance (HList s,Apply a s s') => Apply I (HCons a s) s' where
	apply _ (HCons a s) = apply a s

data Times
times :: Times
times = undefined
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
	Apply r2 (HCons (Seq (Seq (Seq (Seq (Lit b) (Lit t)) (Lit r1)) (Lit r2)) Genrec) s') s'')
	=> HGenrec HFalse r1 r2 b t s s'' where
	hGenrec _ r1 r2 b t s = apply (lit (lit b `o` lit t `o` lit r1 `o` lit r2 `o` genrec) `o` r2) (apply r1 s)

data Genrec
genrec :: Genrec
genrec = undefined
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
instance (Apply b s s',HHead s' b',HLinrec b' b t r1 r2 s s'') => Apply Linrec (HCons r2 (HCons r1 (HCons t (HCons b s)))) s'' where
	apply _ (HCons r2 (HCons r1 (HCons t (HCons b s)))) = hLinrec (hHead (apply b s :: s') :: b') b t r1 r2 s

data Fact1
fact1 :: Fact1
fact1 = undefined
instance (HList s,Apply (Seq (Seq (Seq (Lit (Seq (Lit HZero) EQ))
	(Lit (Seq Pop (Lit (HSucc HZero)))))
        (Lit (Seq (Seq (Seq (Seq Dup (Lit (HSucc HZero))) Sub) Fact1) Mult)))
        Ifte) s s') => Apply Fact1 s s' where
	apply _ s = apply fac1 s

fac1 = lit (lit hZero `o` eq)
	`o` lit (pop `o` lit hOne)
	`o` lit (dup `o` lit hOne `o` sub `o` fact1 `o` mult)
	`o` ifte

fac2 = lit (lit hOne `o` lit hOne)
	`o` dip `o` lit (dup `o` lit mult `o` dip `o` suc)
	`o` times `o` pop

fac3 = lit nul `o` lit suc `o` lit (dup `o` pre)
	`o` lit (i `o` mult) `o` genrec

fac4 = lit nul `o` lit suc `o` lit (dup `o` pre)
	`o` lit mult `o` linrec

main :: IO ()
main = do
	putStrLn $ show $ apply (lit hThree `o` fac1) hNil
	putStrLn $ show $ apply (lit hThree `o` fac2) hNil
	putStrLn $ show $ apply (lit hThree `o` fac3) hNil
	putStrLn $ show $ apply (lit hThree `o` fac4) hNil
