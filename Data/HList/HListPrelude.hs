{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Basic declarations for typeful heterogeneous lists.

   Excuse the unstructured haddocks: while there are many declarations here
   some are alternative implementations should be grouped, and the definitions
   here are analgous to many list functions in the "Prelude".
 -}

module Data.HList.HListPrelude where

import Data.HList.FakePrelude


-- --------------------------------------------------------------------------
-- * Heterogeneous type sequences
-- The easiest way to ensure that sequences can only be formed with Nil
-- and Cons is to use GADTs
-- The kind [*] is list kind (lists lifted to types)

data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)

instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (HCons x l) = let 'H':'[':s = show l
		       in "H[" ++ show x ++ 
			          (if s == "]" then s else ", " ++ s)

infixr 2 `HCons`


-- --------------------------------------------------------------------------
-- * Basic list functions

hHead :: HList (e ': l) -> e
hHead (HCons x _) = x

hTail :: HList (e ': l) -> HList l
hTail (HCons _ l) = l


{-

-- --------------------------------------------------------------------------
-- * A class for extension

class HExtend e l l' | e l -> l', l' -> e l
 where
  hExtend :: e -> l -> l'

instance HExtend e HNil (HCons e HNil)
 where
  hExtend = HCons

instance HList l => HExtend e (HCons e' l) (HCons e (HCons e' l))
 where
  hExtend = HCons

-}

-- --------------------------------------------------------------------------

-- * Appending HLists

-- | The normal append for comparison

append :: [a] -> [a] -> [a]
append [] l = l
append (x:l) l' = x : append l l'


type family HAppend (l1 :: [*]) (l2 :: [*]) :: [*]
type instance HAppend '[] l = l
type instance HAppend (e ': l) l' = e ': HAppend l l'

hAppend :: HList l1 -> HList l2 -> HList (HAppend l1 l2)
hAppend HNil l = l
hAppend (HCons x l) l' = HCons x (hAppend l l')

-- The original HList code is given below. In both cases
-- we had to program the algorithm twice, at the term and the type levels.
{-
-- | The class HAppend

class HAppend l l' l'' | l l' -> l''
 where
  hAppend :: l -> l' -> l''


-- | The instance following the normal append

instance HList l => HAppend HNil l l
 where
  hAppend HNil l = l

instance (HList l, HAppend l l' l'')
      => HAppend (HCons x l) l' (HCons x l'')
 where
  hAppend (HCons x l) l' = HCons x (hAppend l l')
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


-- **  Building lists

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

test_build1 = let x = hBuild True in hEnd x
-- H[True]

test_build2 = let x = hBuild True 'a' in hEnd x
-- H[True, 'a']

test_build3 = let x = hBuild True 'a' "ok" in hEnd x
-- H[True, 'a', "ok"]

{- $hbuild

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

-- * A heterogeneous apply operator

class Apply f a where
  type ApplyR f a :: *
  apply :: f -> a -> ApplyR f a
  apply = undefined                     -- In case we use Apply for
                                        -- type-level computations only


-- | Normal function application

instance Apply (x -> y) x where
  type ApplyR (x -> y) x = y
  apply f x = f x


-- --------------------------------------------------------------------------

-- * A heterogeneous fold for all types
-- GADTs and type-classes mix well

class HFoldr f v (l :: [*]) where
    type HFoldrR f v l :: *
    hFoldr :: f -> v -> HList l -> HFoldrR f v l

instance HFoldr f v '[] where
    type HFoldrR f v '[] = v
    hFoldr       _ v _   = v

instance (Apply f (e, HFoldrR f v l), HFoldr f v l)
    => HFoldr f v (e ': l) where
    type HFoldrR f v (e ': l) = ApplyR f (e, HFoldrR f v l)
    hFoldr f v (HCons x l)    = apply  f (x, hFoldr  f v l)


-- * A heterogeneous unfold for all types

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


-- --------------------------------------------------------------------------
-- * Map
-- It could be implemented with hFoldR, as we show further below

class HMap f (l :: [*]) where
  type HMapR f l :: [*]
  hMap :: f -> HList l -> HList (HMapR f l)

instance HMap f '[] where
  type HMapR f '[] = '[]
  hMap       _  _  = HNil

instance (Apply f e, HMap f l) => HMap f (e ': l) where
  type HMapR f (e ': l) = ApplyR f e ': HMapR f l
  hMap f (HCons x l)    = apply f x `HCons` hMap f l


-- --------------------------------------------------------------------------
-- * Map a heterogeneous list to a homogeneous one

-- This one we implement via hFoldr

newtype Mapcar f = Mapcar f

instance (l ~ [ApplyR f e], Apply f e) => Apply (Mapcar f) (e, l) where
    type ApplyR (Mapcar f) (e, l) = [ApplyR f e]
    apply (Mapcar f) (e, l) = apply f e : l


-- A synonym for the complex constraint
type HMapOut f l e = (HFoldr (Mapcar f) [e] l, HFoldrR (Mapcar f) [e] l ~ [e])

hMapOut :: forall f e l. HMapOut f l e => f -> HList l -> [e]
hMapOut f l = hFoldr (Mapcar f) ([]::[e]) l


-- --------------------------------------------------------------------------
-- * A heterogenous version of mapM.
--
-- > mapM :: forall b m a. (Monad m) => (a -> m b) -> [a] -> m [b]
--
-- Likewise for mapM_.
--
-- See "Data.HList.HSequence" if the result list should also be heterogenous.

hMapM   :: (Monad m, HMapOut f l (m e)) => f -> HList l -> [m e]
hMapM f =  hMapOut f

-- GHC doesn't like its own type.
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

-- * A reconstruction of append

append' :: [a] -> [a] -> [a]
append' l l' = foldr (:) l' l

-- | Alternative implementation of 'hAppend'. Demonstrates 'HFoldr'
hAppend' :: (HFoldr FHCons v l) => HList l -> v -> HFoldrR FHCons v l
hAppend' l l' = hFoldr FHCons l' l

data FHCons = FHCons

instance Apply FHCons (e,HList l) where
    type  ApplyR FHCons (e,HList l) =  HList (e ': l)
    apply _ (e,l) = HCons e l


-- --------------------------------------------------------------------------

-- * A heterogeneous map for all types

newtype MapCar f = MapCar f

-- | Same as 'hMap' only a different implementation.
hMap' :: (HFoldr (MapCar f) (HList '[]) l) => 
    f -> HList l -> HFoldrR (MapCar f) (HList '[]) l
hMap' f = hFoldr (MapCar f) HNil

instance Apply f e => Apply (MapCar f) (e,HList l) where 
    type ApplyR (MapCar f) (e,HList l) = HList (ApplyR f e ': l)
    apply (MapCar f) (e,l) = HCons (apply f e) l


-- --------------------------------------------------------------------------
-- * A function for showing

data HShow  = HShow
newtype HSeq x = HSeq x

instance Show x => Apply HShow x where
  type ApplyR HShow x = IO ()
  apply _ x = putStrLn $ show x

instance (Monad m, ApplyR f x ~ m (), Apply f x) => 
    Apply (HSeq f) (x,m ()) where
  type ApplyR (HSeq f) (x,m ()) = m ()
  apply (HSeq f) (x,c) = do apply f x; c


-- --------------------------------------------------------------------------

-- * Type-level equality for lists

instance HEq '[] '[]      True
instance HEq '[] (e ': l) False
instance HEq (e ': l) '[] False
instance (HEq e1 e2 b1, HEq l1 l2 b2, br ~ HAnd b1 b2)
      => HEq (e1 ': l1) (e2 ': l2) br

-- --------------------------------------------------------------------------
-- * Ensure a list to contain HNats only
-- We do so constructively, converting the HList whose elements
-- are Proxy HNat to [HNat]. The latter kind is unpopulated and
-- is present only at the type level.

type family HNats (l :: [*]) :: [HNat]
type instance HNats '[] = '[]
type instance HNats (Proxy n ': l) = n ': HNats l

hNats :: HList l -> Proxy (HNats l)
hNats = undefined


-- --------------------------------------------------------------------------
-- * Membership tests

-- Check to see if an HList contains an element with a given type
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
type instance HMemberP pred e1 (e ': l) =
    HMemberP' pred e1 l (ApplyR pred (e1,e))

type family HMemberP' pred e1 (l :: [*]) pb :: Bool
type instance HMemberP' pred e1 l (Proxy True) = True
type instance HMemberP' pred e1 l (Proxy False) = HMemberP pred e1 l
 

hMember :: HMember e l b => e -> HList l -> Proxy b
hMember = undefined

-- ** Another type-level membership test
--
-- Check to see if an element e occurs in a list l
-- If not, return 'Nothing
-- If the element does occur, return 'Just l1
-- where l1 is a type-level list without e
{-
class HMemberM e1 (l :: [*]) (r :: Maybe [*]) | e1 l -> r
instance HMemberM e1 '[] 'Nothing
instance (HEq e1 e b, HMemberM' b e1 (e ': l) res)
      =>  HMemberM e1 (e ': l) res
class HMemberM' b e1 (l :: [*]) r | b e1 l -> r
instance HMemberM' True e1 (e ': l) ('Just l)
instance (HMemberM e1 l r, HMemberM' r e1 (e ': l) res)
    => HMemberM' False e1 (e ': l) res
instance HMemberM' Nothing e1 l Nothing
instance HMemberM' (Just l1) e1 (e ': l) (Just (e ': l1))
-}

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


-- * Find an element in a set based on HEq
class HNat n => HFind e l n | e l -> n
 where
  hFind :: e -> l -> n

instance ( HEq e e' b
         , HFind' b e l n
         )
      =>   HFind e (HCons e' l) n
 where
  hFind e (HCons e' l) = n
   where
    b  = hEq e e'
    n  = hFind' b e l

class HNat n => HFind' b e l n | b e l -> n
 where
  hFind' :: b -> e -> l -> n

instance HFind' HTrue e l HZero
 where
  hFind' _ _ _ = hZero

instance HFind e l n
      => HFind' HFalse e l (HSucc n)
 where
  hFind' _ e l = hSucc (hFind e l)





-- ** Membership test based on type equality

class HBool b => HTMember e l b | e l -> b
instance HTMember e HNil HFalse
instance (TypeEq e e' b, HTMember e l b', HOr b b' b'')
      =>  HTMember e (HCons e' l) b''

hTMember :: HTMember e l b => e -> l -> b
hTMember _ _ = undefined


-- * Intersection based on HTMember

class HTIntersect l1 l2 l3 | l1 l2 -> l3
 where
  -- | Like 'Data.List.intersect'
  hTIntersect :: l1 -> l2 -> l3

instance HTIntersect HNil l HNil
 where
  hTIntersect _ _ = HNil

instance ( HTMember h l1 b
         , HTIntersectBool b h t l1 l2
         )
         => HTIntersect (HCons h t) l1 l2
 where
  hTIntersect (HCons h t) l1 = hTIntersectBool b h t l1
   where
    b = hTMember h l1

class HBool b => HTIntersectBool b h t l1 l2 | b h t l1 -> l2
 where
 hTIntersectBool :: b -> h -> t -> l1 -> l2

instance HTIntersect t l1 l2
      => HTIntersectBool HTrue h t l1 (HCons h l2)
 where
  hTIntersectBool _ h t l1 = HCons h (hTIntersect t l1)

instance HTIntersect t l1 l2
      => HTIntersectBool HFalse h t l1 l2
 where
  hTIntersectBool _ _ t l1 = hTIntersect t l1


-- * Turn a heterogeneous list into a homogeneous one

-- | Same as @hMapOut Id@
class HList2List l e
 where
  hList2List :: l -> [e]

instance HList2List HNil e
 where
  hList2List HNil = []

instance HList2List l e
      => HList2List (HCons e l) e
 where
  hList2List (HCons e l) = e:hList2List l


-- --------------------------------------------------------------------------
-- * With 'HMaybe'

-- ** Turn list in a list of justs

class ToHJust l l' | l -> l'
 where
  toHJust :: l -> l'

instance ToHJust HNil HNil
 where
  toHJust HNil = HNil

instance ToHJust l l' => ToHJust (HCons e l) (HCons (HJust e) l')
 where
  toHJust (HCons e l) = HCons (HJust e) (toHJust l)


-- --------------------------------------------------------------------------
-- ** Extract justs from list of maybes

class FromHJust l l' | l -> l'
 where
  fromHJust :: l -> l'

instance FromHJust HNil HNil
 where
  fromHJust HNil = HNil

instance FromHJust l l' => FromHJust (HCons HNothing l) l'
 where
  fromHJust (HCons _ l) = fromHJust l

instance FromHJust l l' => FromHJust (HCons (HJust e) l) (HCons e l')
 where
  fromHJust (HCons (HJust e) l) = HCons e (fromHJust l)


-- --------------------------------------------------------------------------
-- * Annotated lists

data HAddTag t = HAddTag t
data HRmTag    = HRmTag

hAddTag :: (HMap (HAddTag t) l l') => t -> l -> l'
hAddTag t l = hMap (HAddTag t) l
hRmTag :: (HMap HRmTag l l') => l -> l'
hRmTag l    = hMap HRmTag l

instance Apply (HAddTag t) e (e,t)
 where
  apply (HAddTag t) e = (e,t)

instance Apply HRmTag (e,t) e
 where
  apply HRmTag (e,_) = e


-- | Annotate list with a type-level Boolean
hFlag :: (HMap (HAddTag HTrue) l l') => l -> l'
hFlag l = hAddTag hTrue l


-- --------------------------------------------------------------------------
-- * Splitting by HTrue and HFalse

-- | Analogus to @Data.List.partition snd@

class HSplit l l' l'' | l -> l' l''
 where
  hSplit :: l -> (l',l'')

instance HSplit HNil HNil HNil
 where
  hSplit HNil = (HNil,HNil)

instance HSplit l l' l''
      => HSplit (HCons (e,HTrue) l) (HCons e l') l''
 where
  hSplit (HCons (e,_) l) = (HCons e l',l'')
   where
    (l',l'') = hSplit l

instance HSplit l l' l''
      => HSplit (HCons (e,HFalse) l) l' (HCons e l'')
 where
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

-}
