{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Needed for a reply to the Haskell mailing list

import Data.HList.CommonMain hiding (Comp(..))

main = do
    print $ comp "abc"
    print $ (hComposeList test2 "abc" :: Int) -- definition in HList now

test = HCons (length::String -> Int) (HCons ((+1)::(Int->Int)) (HCons ((*2)::(Int->Int)) HNil))
test2 = length .*. (+1) .*. (*2) .*. HNil

data Comp

{- simpler class. wouldn't work with test2. The original HFoldr won't work with
 - Apply anymore.
instance Apply Comp (x -> y,y -> z)
 where
  type ApplyR Comp (x -> y,y -> z) = x -> z
  apply _ (f,g) = g . f
  -}

instance ((x -> y,y -> z) ~ xyz, (x -> z) ~ xz)
    => ApplyAB Comp xyz xz
 where
  applyAB _ (f,g) = g . f

-- Function composition based on type code works.

comp  = hFoldr (undefined::Comp) (id::Int -> Int) test

-- Function composition based on normal polymorphism doesn't
-- comp' = hFoldr (uncurry (flip (.))) (id::Int -> Int) test

{-

From Ralf.Lammel at microsoft.com  Mon Nov  7 00:11:01 2005
From: Ralf.Lammel at microsoft.com (Ralf Lammel)
Date: Sun Nov  6 23:50:27 2005
Subject: [Haskell-cafe] Type classes and hFoldr from HList
Message-ID: <1152E22EE8996742A7E36BBBA7768FEE079C474F@RED-MSG-50.redmond.corp.microsoft.com>

Hi Greg,

Since hfoldr is right-associative, I prefer to reorder your list of
functions as follows:

> test = HCons (length::String -> Int) (HCons ((+1)::(Int->Int)) (HCons
((*2)::(Int->Int)) HNil))

Note that I also annotated length with its specific type.
(If you really wanted to leave things more polymorphic, you would need
to engage in TypeCast.)

Providing a specific Apply instance for (.) is not necessary, strictly
necessary. We could try to exploit the normal function instance for
Apply.

Let me recall that one here for convenience:

>instance Apply (x -> y) x y
> where
>  apply f x = f x

Let me also recall the hFoldr instances:

>class HList l => HFoldr f v l r | f v l -> r
> where
>  hFoldr :: f -> v -> l -> r

>instance HFoldr f v HNil v
> where
>  hFoldr _ v _ = v

>instance ( HFoldr f v l r
>         , Apply f (e,r) r'
>         )
>      => HFoldr f v (HCons e l) r'
> where
>  hFoldr f v (HCons e l) = apply f (e,hFoldr f v l)


To fit in (.), we would flip and uncurry it.
So we could try:

comp' = hFoldr (uncurry (flip (.))) (id::Int -> Int) test

This wouldn't work.
The trouble is the required polymorphism of the first argument of
hFoldr.
The type of that argument as such is polymorphic.
However, this polymorphism does not survive type class parameterization.
You see this by looking at the HCons instance of HFoldr.
The different occurrences of "f" would need to be used at different
types.
This would only work if the type class parameter f were instantiated by
the polymorphic type of (uncurry (flip (.))). (And even then we may need
something like TypeCast.)

What you can do is define a dedicated *type code* for composition.

comp  = hFoldr (undefined::Comp) (id::Int -> Int) test

data Comp

instance Apply Comp (x -> y,y -> z) (x -> z)
 where
  apply _ (f,g) = g . f


Ralf


> -----Original Message-----
> From: haskell-cafe-bounces@haskell.org [mailto:haskell-cafe-
> bounces@haskell.org] On Behalf Of Greg Buchholz
> Sent: Sunday, November 06, 2005 7:01 PM
> To: haskell-cafe@haskell.org
> Subject: [Haskell-cafe] Type classes and hFoldr from HList
> 
> 
>   I was playing around with the HList library from the paper...
> 
>     Strongly typed heterogeneous collections
>     http://homepages.cwi.nl/~ralf/HList/
> 
> ...and I thought I'd try to fold the composition function (.) through
a
> heterogeneous list of functions, using hFoldr...
> 
> >{-# OPTIONS -fglasgow-exts #-}
> >{-# OPTIONS -fallow-undecidable-instances #-}
> >
> >import CommonMain
> >
> >main = print $ comp "abc"
> >
> >test = HCons ((+1)::(Int->Int)) (HCons ((*2)::(Int->Int)) (HCons
length
> HNil))
> >
> >comp = hFoldr (.) id test
> >
> >instance Apply (a -> b -> c -> d) (a, b) (c -> d)
> >    where
> >        apply f (a,b) = f a b
> 
> ...but it fails with the following type error...
> 
> ]Compiling Main             ( compose.hs, interpreted )
> ]
> ]compose.hs:10:7:
> ]    No instances for (Apply ((b -> c) -> (a -> b) -> a -> c)
> ]                            (Int -> Int, r)
> ]                            ([Char] -> a3),
> ]                      Apply ((b -> c) -> (a -> b) -> a -> c) (Int ->
Int,
> r1) r,
> ]                      Apply ((b -> c) -> (a -> b) -> a -> c) ([a2] ->
> Int, a1 ->a1) r1)
> ]      arising from use of `hFoldr' at compose.hs:10:7-12
> ]    Probable fix:
> ]      add an instance declaration for (Apply ((b -> c) -> (a -> b) ->
a -
> > c)
> ]                                             (Int -> Int, r)
> ]                                             ([Char] -> a3),
> ]                                       Apply ((b -> c) -> (a -> b) ->
a -
> > c)
> ](Int -> Int, r1) r,
> ]                                       Apply ((b -> c) -> (a -> b) ->
a -
> > c)
> ]([a2] -> Int, a1 -> a1) r1)
> ]    In the definition of `comp': comp = hFoldr (.) id test
> 
> ...Anyway, I couldn't quite tell whether I was using hFoldr
incorrectly,
> or if I needed to have more constraints placed on the construction of
> "test", or if needed some sort of type-level function that resolves...
> 
> Apply ((b -> c) -> (a -> b) -> a -> c)
> 
> ...into (a -> c), or something else altogether.  I figured someone
might
> be able to help point me in the right direction.

-}
