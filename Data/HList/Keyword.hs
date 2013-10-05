{-# LANGUAGE TypeFamilies, TypeOperators, ScopedTypeVariables, DataKinds, MultiParamTypeClasses,
      FunctionalDependencies, PolyKinds, FlexibleInstances, FlexibleContexts,
      UndecidableInstances, StandaloneDeriving, GADTs, OverlappingInstances #-}

{- |

A couple puzzling issues remain:

* DummyToMakeIsKeyFNBehave

* why can't we have keyword functions defined in lets / top-level anymore?



>originally:
> From oleg-at-okmij.org Fri Aug 13 14:58:35 2004
> To: haskell@haskell.org
> Subject: Keyword arguments
> From: oleg-at-pobox.com
> Message-ID: <20040813215834.F1FF3AB7E@Adric.metnet.navy.mil>
> Date: Fri, 13 Aug 2004 14:58:34 -0700 (PDT)
> Status: OR


We show the Haskell implementation of keyword arguments, which goes
well beyond records (e.g., in permitting the re-use of
labels). Keyword arguments indeed look just like regular, positional
arguments. However, keyword arguments may appear in any
order. Furthermore, one may associate defaults with some keywords; the
corresponding arguments may then be omitted. It is a type error to
omit a required keyword argument. The latter property is in stark
contrast with the conventional way of emulating keyword arguments via
records. Also in marked contrast with records, keyword labels may be
reused throughout the code with no restriction; the same label may be
associated with arguments of different types in different
functions. Labels of Haskell records may not be re-used.  Our solution
is essentially equivalent to keyword arguments of DSSSL Scheme or
labels of OCaml.

Keyword argument functions are naturally polyvariadic: Haskell does
support varargs! Keyword argument functions may be polymorphic. As
usual, functions with keyword arguments may be partially applied. On
the downside, sometimes one has to specify the type of the return
value of the function (if the keyword argument function has no
signature -- the latter is the norm, see below) -- provided that the
compiler cannot figure the return type out on its own. This is usually
only the case when we use keyword functions at the top level (GHCi
prompt).

Our solution requires no special extensions to Haskell and works with
the existing Haskell compilers; it is tested on GHC 6.0.1. The
overlapping instances extension is not necessary (albeit it is
convenient).

The gist of our implementation is the realization that the type of a
function is a polymorphic collection of its argument types -- a
collection that we can traverse. This message thus illustrates a
limited form of the reflection on a function.


Our implementation is a trivial extension of the strongly-typed
polymorphic open records described in
	http://homepages.cwi.nl/~ralf/HList/

In fact, the implementation relies on the HList library.  To run the
code (which this message is), one needs to download the HList library
from the above site.

The HList paper discusses the issue of labels in some detail. The
paper gives three different representations. One of them needs no
overlapping instances and is very portable. In this message, we chose
a representation that relies on generic type equality and therefore
needs overlapping instances as implemented in GHC. Again, this is
merely an outcome of our non-deterministic choice. It should be
emphasized that other choices are possible, which do not depend on
overlapping instances at all. Please see the HList paper for details.

-}
module Data.HList.Keyword where

import Data.HList.FakePrelude
import Data.HList.TypeEqO
import Data.HList.HListPrelude
import Data.HList.HList

-- | why does commenting this instance, which is never used, make
--
-- > doctest Keyword.hs
--
-- fail?
instance IsKeyFN DummyToMakeIsKeyFNBehave True
data DummyToMakeIsKeyFNBehave = DummyToMakeIsKeyFNBehave deriving Show

-- * example
{- $setup

 >>> :set -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses
 >>> :set -XScopedTypeVariables -XOverlappingInstances

We will be using an example inspired by a graphics toolkit -- the area
which really benefits from keyword arguments. We first define our
labels and useful datatypes

 >>> data Color = Color
 >>> data Size  = Size
 >>> data Origin  = Origin
 >>> data RaisedBorder = RaisedBorder


The number of arguments each keyword must be specified by an 'IsKeyFN'
instance.

 >>> instance IsKeyFN (Color->a->b)  True
 >>> instance IsKeyFN (Size->a->b)   True
 >>> instance IsKeyFN (Origin->a->b) True
 >>> instance IsKeyFN (RaisedBorder->a->b) True

Note that if a keyword is to take only a certain argument type, it is possible
to restrict that here

 >>> data CommonColor = Red | Green | Blue deriving Show
 >>> data RGBColor = RGBColor Int Int Int deriving Show

and two functions:

 >>> :{
 let make_square Size n Origin (x0,y0) Color (color::CommonColor) =
        unwords ["Square:", show (n :: Int), "at", show (x0 :: Int,y0 :: Int), show color] ++ "\n"
 :}

 >>> :{
 let make_rect Size (nx,ny) Origin (x0,y0) Color (color::RGBColor)
         RaisedBorder border =
        unwords ["Rectangle:", show (nx,ny), "at", show (x0,y0),
             show color, if border then "raised border" else ""] ++ "\n"
 :}

-}

{- $ex2

We are not interested in really drawing squares and rectangles
here. Therefore, make_square and make_rect return a String, which we
can regard as a ``command'' to be passed to a low-level graphics
library. The functions make_square and make_rect are genuine functions
and can be used as such. They are not keyword argument functions, yet,
but they set the stage. These functions can be considered an
`interface' for the keyword argument functions. We should note that
the functions are polymorphic: for example, `Size' can be any
showable. We must also emphasize the re-use of the labels: The Color
of a square is the value of the enumerated type CommonColor. OTH, the
color of the rectangle is given as an RGB triple. The sizes of the
square and of the rectangle are specified differently too, the same
label notwithstanding.

Once the user wrote the functions such as make_square and make_rect,
he can _automatically_ convert them to their keyword
alternatives. This transformation is done by a function 'kw'. The user
should pass the positional-argument function (`labeled' as above),
and an HList of default values for some of the labels. The latter may
be HNil if all keyword arguments are required.

The first example (no defaults)

 >>> kw make_square HNil Size (1::Int) Origin (0::Int,10::Int) Color Red   :: String
 "Square: 1 at (0,10) Red\n"

we can permute the arguments at wish

 >>> kw make_square HNil Color Red Size (1::Int) Origin (0::Int,10::Int)   :: String
 "Square: 1 at (0,10) Red\n"

we can also assign a name to a keyword function, or partially apply it:

 >>> :{
 case kw make_square HNil Color Red of
    f -> "here: " ++ f Origin (0::Int,10::Int) Size (1::Int)
:}
"here: Square: 1 at (0,10) Red\n"

Note that it is necessary to use a monomorphic pattern binding here (lambda or
case). The following does not work with ghc-7.6, since instance selection
is now supposed to happen at the `f' instead of later on:

> let f = kw make_square HNil Color Red
> in "here: " ++ f Origin (0::Int,10::Int) Size (1::Int)


The following is a more interesting example, with the
defaults:

 >>> :{
let defaults = Origin .*. (0::Int,10::Int) .*.
             RaisedBorder .*. True .*.
             HNil
    in kw make_square defaults Size (1::Int) Color Red ++
       kw make_rect   defaults Color (RGBColor 0 10 255)
                               Size (1.0::Float, 2.0::Float)
:}
"Square: 1 at (0,10) Red\nRectangle: (1.0,2.0) at (0,10) RGBColor 0 10 255 raised border\n"

The argument RaisedBorder is not given, and so the default value is
used. Of course, we can override the default:

 >>> :{
let defaults = Origin .*. (0::Int,10::Int) .*.
                    RaisedBorder .*. True .*.
                    HNil
 in case kw make_square defaults Color of
     sq -> case kw make_rect   defaults of
      re ->
         sq Red Size (1::Int) ++
         re Color (RGBColor 0 10 255)
             RaisedBorder False
             Size (1.0::Float, 2.0::Float)
:}
"Square: 1 at (0,10) Red\nRectangle: (1.0,2.0) at (0,10) RGBColor 0 10 255 \n"

We have reshuffled a few arguments, just for fun. As you can see, the
function `kw make_rect defaults' is polyvariadic indeed.  We chose to
partially apply 'Color' to the function `kw make_square defaults' --
so that the function `sq' is positional in its first argument, and
keyword in the rest.

If we omit a required argument, we get a type error:

> ] testse1 = let f x = kw make_square HNil Color Red x
> ] 	    in "here: " ++ f Origin (0::Int,10::Int)
>
>   Couldn't match `ErrReqdArgNotFound Size' against `[Char]'
>       Expected type: ErrReqdArgNotFound Size
>       Inferred type: [Char] ...

The error message seems reasonably clear. Likewise we get an error
message if we pass to a keyword function an argument it does not expect:

> ] testse2 = let f x = kw make_square HNil Color Red x
> ] 	    in "here: " ++ f Origin (0::Int,10::Int) Size (1::Int)
> ]	                   RaisedBorder False
>
>   No instances for (Fail (ErrUnexpectedKW RaisedBorder),
> 		    KWApply [Char] (HCons RaisedBorder (:*: Bool HNil)) [Char])
>       arising from use of `f' at ...
>     In the second argument of `(++)', namely
>   `f Origin (0 :: Int, 10 :: Int) Size (1 :: Int) RaisedBorder False'


The function 'kw' receives the appropriately labeled function (such
as make_square) and the HList with default values. The function 'kw'
is polymorphic; the overloading is resolved based on the type of the
user function *and* on the type of its continuation. The continuation
indicates if a keyword argument is forthcoming, or not. In the latter
case, 'kw' checks to see if the supplied defaults can provide the
values of the still missing arguments. We see therefore that a
function type is more than it may appear -- the type of a function is
truly a heterogeneous, type level collection! The function 'kw'
traverses that collection, thus performing a limited form of
reflection on Haskell functions.


Implementation Outline

One of the key tools of the implementation is 'kwapply', which applies
a function to a polymorphic collection of that function's arguments.
The order of the arguments in the collection is irrelevant. The
contraption kwapply can handle polymorphic functions with arbitrary
number of labeled arguments.

For example, if we define

> f1 Size n = show n
> f2 Size n Color m = unwords ["size:", show n, "color:", show m]
> f3 Origin x Color m Size n =
>     unwords ["origin:", show x, "size:", show n, "color:",show m]

then we can run

> katest1  = kwapply f1 (Size .*. () .*. HNil)
> katest11 = kwapply f1 (Size .*. "Large" .*. HNil)
>
> katest2  = kwapply f2 (Size .*. (1::Int) .*. Color .*. Red .*. HNil)
> katest21 = kwapply f2 (Color .*. Red .*. Size .*. (1::Int) .*.  HNil)
>
> katest3  = kwapply f3 (Size .*. (1::Int) .*. Origin .*. (2.0::Float) .*.
> 		         Color .*. Red .*. HNil)

-}


-- | Another key contraption is

reflect_fk:: (ReflectFK fn kws) => fn -> Arg kws '[]
reflect_fk _ = Arg HNil

{- ^

that reflects on a user-supplied function. It converts the *type* of a
user function to a collection of keywords required by that
function. This and the previous contraptions may be used to define an
`extended' version of some user function that takes more arguments --
without the need to enumerate all arguments of the original
function. We thus infringe on the area of object and module systems.

The rest of the implementation is just to convert `kw fn defaults'
into the application of kwapply.

-}


-- * The rest of the implementation
{- $impl

We should note that all implementation is written in the
continuation-passing style (CPS) -- at the term level and especially
at the _typeclass level_. One of the reasons is to avoid relying on
overlapping instances: we compare types with a predicate `TypeEq x y
hbool', obtain the type-level boolean, and dispatch to two
non-overlapping instances of an auxiliary, continuation class. One
instance handles HTrue, and the other the HFalse alternative. Please
see the HList paper for more discussion of this technique.

The other, equally important reason for the thorough CPS of the
typeclasses is to control the order of evaluation of constraints and
their functional dependencies. The sole reason is to produce
informative error messages. The order of constraints is irrelevant
when all the constraints are satisfied. However, if the user omitted a
required keyword, many of the constraints below will fail. If a
'wrong' constraint fails first, we get a totally off-the-wall error
message that gives us no clue whatsoever about the problem. By tightly
constraining the order via CPS, we are able to force the typechecker
to give informative error messages.

-}



-- * Errors

data ErrReqdArgNotFound x
data ErrUnexpectedKW x
data Trace x

-- | All our keywords must be registered

class IsKeyFN   t (flag :: Bool) | t-> flag
instance (False ~ flag) => IsKeyFN t flag


-- * The implementation of KWApply

class KWApply f arg_values r where
    kwapply:: f -> HList arg_values -> r

instance (r ~ r') => KWApply r '[] r' where
    kwapply f _ = f

instance (HEq kw kw' flag,
	  KWApply' flag (kw->a->f') (kw' ': a' ': tail) r)
    => KWApply (kw->a->f') (kw' ': a' ': tail) r where
    kwapply = kwapply' (proxy :: Proxy flag)

class KWApply' flag f arg_values r  | flag f arg_values -> r  where
    kwapply':: Proxy flag -> f -> HList arg_values -> r

instance  (v' ~ v, KWApply f' tail r)
    => KWApply' True (kw->v->f') (kw ': v' ': tail) r where
    kwapply' _ f (HCons kw (HCons v' tail)) =
                   kwapply (f kw v') tail

-- | Rotate the arg list ...
instance  (HAppendList tail '[kw , v] ~ l',
	   KWApply f l' r)
    => KWApply' False f (kw ': v ': tail) r where
    kwapply' _ f (HCons kw (HCons v tail)) =
	kwapply f (hAppend tail (kw .*. v .*. HNil))

{- |

The datatype Arg below is to maintain the state of keyword
accumulation: which keywords we need, and which keyword and values we
have already got.
arg_types is the phantom HList of keywords that are yet to be satisfied.
arg_values is the @HList (kw .*. kw_value .*. etc)@
of already found keywords and their values.
-}

newtype Arg arg_types arg_values = Arg (HList arg_values)
deriving instance Show (HList vals) => Show (Arg tys vals)

{- | Reflection on a function:
Given a function, return the type list of its keywords

:t reflect_fk (undefined::Size->Int->Color->CommonColor->String)
:t reflect_fk (undefined::Size->Int->()->Int)
-}

class ReflectFK f (kws :: [*]) | f -> kws
instance (IsKeyFN f flag, ReflectFK' flag f kws) => ReflectFK f kws
class ReflectFK' (flag :: Bool) f kws | flag f -> kws
instance ReflectFK rest kws => ReflectFK' True (kw->a->rest) (kw ': kws)
instance ReflectFK' False f '[]


-- | The main class: collect and apply the keyword arguments

class KW f arg_desc arg_def r where
    kwdo :: f -> arg_desc -> HList arg_def -> r

instance (IsKeyFN r rflag, -- Fail (Trace (arg_desc,r)),
	    KW' rflag f arg_desc arg_def r)
    => KW f arg_desc arg_def r where
    kwdo = kw' (proxy ::Proxy rflag)

class KW' rflag f arg_desc arg_def r where
    kw' :: Proxy rflag -> f -> arg_desc -> HList arg_def -> r

{- |
If the continuation r does not promise any more keyword
arguments, apply the defaults -}

instance KWMerge arg_needed arg_values arg_def f r
    => KW' False f (Arg arg_needed arg_values) arg_def r where
    kw' _ f args_given arg_def = kwmerge args_given arg_def f

{- | Otherwise, collect the supplied keyword and its value, and recurse for
more: -}

instance KWAcc arg_desc kw a f arg_def r
    => KW' True f arg_desc arg_def (kw->a->r) where
    kw' _ f arg_desc arg_def kw a = kwaccum arg_desc kw a f arg_def


{- | Add the needed arguments from arg_def to arg_values and continue
with kwapply.

That is, we try to satisfy the missing arguments from the defaults.
It will be a type error if some required arguments are missing -}

class KWMerge arg_needed arg_values arg_def f r where
    kwmerge:: Arg arg_needed arg_values -> HList arg_def -> f -> r

instance KWApply f arg_values r
    => KWMerge '[] arg_values arg_def f r where
    kwmerge (Arg arg_values) _ f = kwapply f arg_values

instance KWMerge' kw arg_def atail arg_values arg_def f r
    => KWMerge (kw ': atail) arg_values arg_def f r where
    kwmerge (Arg arg_values) arg_def =
	kwmerge' (undefined :: kw) arg_def
	         ((Arg arg_values)::Arg atail arg_values) arg_def

class KWMerge' kw list atail arg_values arg_def f r where
    kwmerge':: kw -> HList list -> (Arg atail arg_values) -> HList arg_def -> f -> r

instance (Fail (ErrReqdArgNotFound kw), nff ~ (ErrReqdArgNotFound kw))
    => KWMerge' kw '[] atail arg_values arg_def f
                nff where
    kwmerge' = undefined
instance (HEq kw kw' flag,
	  KWMerge'' flag kw (kw' ': etc) atail arg_values arg_def f r)
    => KWMerge' kw (kw' ': etc) atail arg_values arg_def f r where
    kwmerge' = kwmerge'' (undefined:: Proxy flag)

class KWMerge'' (flag :: Bool) kw (list :: [*]) atail arg_values arg_def f r
     where
    kwmerge'':: Proxy flag -> kw -> HList list
        -> Arg atail arg_values -> HList arg_def
		-> f -> r
instance KWMerge atail (kw ': v ': arg_values) arg_def f r
    => KWMerge'' True kw (kw ': v ': tail)
                 atail arg_values arg_def f r where
    kwmerge'' _ _ (HCons kw (HCons v _)) (Arg arg_values) =
	kwmerge ((Arg (kw .*. v .*. arg_values))::
		 (Arg atail (kw ': v ': arg_values)))
instance KWMerge' kw tail atail arg_values arg_def f r
    => KWMerge'' False kw (kw' ': v' ': tail)
                 atail arg_values arg_def f r where
    kwmerge'' _ kw (HCons _ (HCons _ tail)) = kwmerge' kw tail

-- | Add the real argument to the Arg structure, and continue

class KWAcc arg_desc kw a f arg_def r where
    kwaccum:: arg_desc -> kw -> a -> f -> HList arg_def -> r


instance (HDelete kw arg_types arg_types',
	  KW f (Arg arg_types' (kw ': a ': arg_values)) arg_def r)
    => KWAcc (Arg arg_types arg_values) kw a f arg_def r  where
    kwaccum (Arg arg_values) kw a f =
	kwdo f (Arg (kw .*. a .*. arg_values)::
		Arg arg_types' (kw ': a ': arg_values))


-- | Delete e from l to yield l' The element e must occur in l

class HDelete e (l :: [k]) (l' :: [k])
instance (Fail (ErrUnexpectedKW e), r ~ '[]) => HDelete e '[] r
instance (HEq e e' flag, HDelete' flag e (e' ': tail) l')
    => HDelete e (e' ': tail) l'
class HDelete' (flag :: Bool) e l l'
instance (tail' ~ tail) => HDelete' True e (e ': tail) tail'
instance (HDelete e tail tail', e'tail ~ (e' ': tail'))
    => HDelete' False e (e' ': tail) e'tail

-- | Finally,

kw f = kwdo f (reflect_fk f)


