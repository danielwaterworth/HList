{-# LANGUAGE CPP #-}
{- |

Description : labels which are also lenses (or prisms)

A simple problem is being solved here, but unfortunately it
is a bit involved. The idea is to use the same haskell identifier
for a lens and for other purposes. In other words, get the same
behavior as:

 > x = hLens (Label :: Label "x")
 > r ^. x

While still being able to extract the symbol \"x\" from x, so that things
like @x .=. 123@ could be acceptable. In this case we don't overload '.=.',
so instead you have to write @x .==. 123@.


Elaboration of some ideas from edwardk.
-}
module Data.HList.Labelable
    (Labelable(..),
     LabeledOptic,
     (.==.),

    -- * comparison with 'hLens'
    -- $comparisonWithhLensFunction

    -- * likely unneeded (re)exports
    LabeledCxt1,
    LabeledTo(LabeledTo),
    LabeledR(LabeledR),
    toLabel,
    Identity,
    LabelableTIPCxt,
    LabeledOpticType(..),
    ) where


import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.Variant
import Data.HList.TIP
import Data.HList.TIC

import Control.Monad.Identity
import GHC.TypeLits
import LensDefs
import GHC.Exts (Constraint)

{- | This alias is the same as Control.Lens.Optic, except the (->) in Optic
is a type parameter 'to' in LabeledOptic.

Depending on the collection type (see instances of 'LabelableTy'),
the type variables @to, p, f@ are constrained such that the resulting
type is a @Lens (r s) (r t) a b@, @Prism (r s) (r t) a b@ or a
@LabeledTo x _ _@. The latter can be used to recover the label (@x@) when
used as an argument to '.==.' or equivalently 'toLabel'.
-}
type LabeledOptic (x :: k) (r :: [*] -> *) (s :: [*]) (t :: [*]) (a :: *) (b :: *)
    = forall ty to p f.
                     (ty ~ LabelableTy r,
                      LabeledOpticF ty f,
                      LabeledOpticP ty p,
                      LabeledOpticTo ty x to) => (a `p` f b) `to` (r s `p` f (r t))

data LabeledOpticType = LabelableLens | LabelablePrism | LabelableLabel

type family LabeledOpticF (ty :: LabeledOpticType) :: (* -> *) -> Constraint
type instance LabeledOpticF LabelableLens = Functor
type instance LabeledOpticF LabelablePrism = Applicative
type instance LabeledOpticF LabelableLabel = (~) Identity

type family LabeledOpticP (ty :: LabeledOpticType) :: (* -> * -> *) -> Constraint
type instance LabeledOpticP LabelableLens = (~) (->)
type instance LabeledOpticP LabelablePrism = Choice
type instance LabeledOpticP LabelableLabel = (~) (->)

type family LabeledOpticTo (ty :: LabeledOpticType) (x :: k) :: (* -> * -> *) -> Constraint
type instance LabeledOpticTo LabelableLens x = (~) (->)
type instance LabeledOpticTo LabelablePrism x = (~) (->)
type instance LabeledOpticTo LabelableLabel x = (~) (LabeledTo x)


{- |

[@r@] is 'Record', 'Variant'. 'TIP' and 'TIC' also have instances, but generally
'tipyLens'' and 'ticPrism'' are more appropriate.

[@x@] is the label for the field. It tends to have kind 'GHC.TypeLits.Symbol',
but others are supported in principle.

-}
class SameLength s t => Labelable (x :: k) (r :: [*] -> *) s t a b
          | x s -> a, x t -> b,    -- lookup
            x s b -> t, x t a -> s -- update
  where
    type LabelableTy r :: LabeledOpticType
    hLens' :: Label x -> LabeledOptic x r s t a b

data LabeledTo (x :: k) (a :: *) (b :: *) = LabeledTo deriving (Show)

data LabeledR (x :: [*]) = LabeledR


-- | make a @Lens (Record s) (Record t) a b@
instance HLens x Record s t a b
        => Labelable x Record s t a b where
            type LabelableTy Record = LabelableLens
            hLens' = hLens

-- | used with 'toLabel' and/or '.==.'
instance LabeledCxt1 s t a b => Labelable x LabeledR s t a b where
        type LabelableTy LabeledR = LabelableLabel
        hLens' _ = LabeledTo

-- | sets all type variables to dummy values: only the @Labeled x@
-- part is actually needed
type LabeledCxt1 s t a b = (s ~ '[], t ~ '[], a ~ (), b ~ ())

-- | make a @Prism (Variant s) (Variant t) a b@
instance (HPrism x s t a b,
          to ~ (->)) => Labelable x Variant s t a b where
    type LabelableTy Variant = LabelablePrism
    hLens' x s = hPrism x s

-- | @hLens' :: Label a -> Prism' (TIC s) a@
--
-- note that a more general function @'ticPrism' :: Prism (TIC s) (TIC t) a b@,
-- cannot have an instance of Labelable
instance (TICPrism s t a b, x ~ a, a ~ b, s ~ t,
          SameLength s t) =>
    Labelable (x :: *) TIC s t a b where
      type LabelableTy TIC = LabelablePrism
      hLens' _ = ticPrism


-- | make a @Lens' (TIP s) a@.
--
-- 'tipyLens' provides a @Lens (TIP s) (TIP t) a b@, which tends to need
-- too many type annotations to be practical
instance LabelableTIPCxt x s t a b =>
    Labelable x TIP s t a b where
    type LabelableTy TIP = LabelableLens
    hLens' = hLens

type LabelableTIPCxt x s t a b =
     (s ~ t, a ~ b, x ~ a,
      HLens x TIP s t a b)


-- | modification of '.=.' which works with the labels from this module,
-- and those from "Data.HList.Label6". Note that this is not strictly a
-- generalization of '.=.', since it does not work with labels like
-- "Data.HList.Label3" which have the wrong kind.
l .==. v = toLabel l .=. v

infixr 4 .==.


{- | Create a @'Label' (x :: 'Symbol')@:

> toLabel :: LabeledTo x _ _ -> Label x
> toLabel :: Label x         -> Label x
> toLabel :: Proxy x         -> Label x

-}
class ToSym label (s :: Symbol) | label -> s where
    toLabel :: label -> Label s

instance LabeledTo x (a `p` f b) (LabeledR s `p` f (LabeledR t)) ~ v1 v2 v3
    => ToSym (v1 v2 v3) x where
      toLabel _ = Label

instance ToSym (label x) x where
    toLabel _ = Label


{- $comparisonWithhLensFunction

Note that passing around variables defined with 'hLens'' doesn't get
you exactly the same thing as calling 'hLens' at the call-site:

The following code needs to apply the @x@ for different @Functor
f =>@, so you would have to write a type signature (rank-2) to allow this
definition:

 > -- with the x defined using hLens'
 > let f x r = let
 >          a = r ^. x
 >          b = r & x .~ "6"
 >        in (a,b)

This alternative won't need a type signature

 > -- with the x defined as x = Label :: Label "x"
 > let f x r = let
 >          a = r ^. hLens x
 >          b = r & hLens x .~ "6"
 >        in (a,b)

It may work to use 'hLens'' instead of 'hLens' in the second code,
but that is a bit beside the point being made here.

The same points apply to the use of 'hPrism' over 'hLens''.

-}
