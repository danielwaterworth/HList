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
    (Labelable(hLens'),
     LabeledOptic,
     (.==.),

    -- * comparison with 'hLens'
    -- $comparisonWithhLensFunction

    -- * likely unneeded (re)exports
    -- $note needed to make a needed instance visible
    LabeledCxt, LabeledCxt1,
    Labeled(Labeled),
    toLabel,
    Identity,
    ToSym,
    ) where


import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.Variant
import Data.HList.TIP

import Control.Monad.Identity
import GHC.TypeLits

import Language.Haskell.TH

{- | This alias is the same as Control.Lens.Optic, except the (->) in Optic
is a type parameter 'to' in LabeledOptic. Usually \"to\" is @->@, but it
can also be set to @Labeled x@ to recover that type parameter when used as
an argument to '.==.' or equivalently 'toLabel'
-}
type LabeledOptic (to :: * -> * -> *)
                  (p :: * -> * -> *)
                  (f :: * -> *)
                  s t a b = (a `p` f b) `to` (s `p` f t)

{- |

[@r@] is 'Record' or 'Variant'

[@x@] is the label for the field. It tends to have kind 'GHC.TypeLits.Symbol',
but others are supported in principle.

-}
class SameLength s t =>
    Labelable (x :: k) (r :: [*] -> *) (to :: * -> * -> *)
          p (f :: * -> *)
          s t a b
          | x s -> a, x t -> b,    -- lookup
            x s b -> t, x t a -> s -- update
  where
    hLens' :: Label x -> LabeledOptic to p f (r s) (r t) a b

data Labeled (x :: k) (a :: *) (b :: *) = Labeled deriving (Show)


-- | make a @Lens (Record s) (Record t) a b@
instance (Functor f,
          HUpdateAtLabel Record x b s t,
          HUpdateAtLabel Record x a t s,
          SameLength s t,
          (->) ~ to,
          (->) ~ p)
        => Labelable x Record to p f s t a b where
            hLens' lab f rec = fmap (\v -> hUpdateAtLabel lab v rec) (f (rec .!. lab))

-- | used with 'toLabel' and/or '.==.'
instance LabeledCxt1 x' r (Labeled x) p f s t a b
    => Labelable x' r (Labeled x) p f s t a b where
        hLens' _ = Labeled :: LabeledOptic (Labeled x) p f (r s) (r t) a b


-- | sets all type variables to dummy values: only the @Labeled x@
-- part is actually needed
type LabeledCxt1 x r to p f s t a b =
        (to ~ Labeled x, f ~ Identity,
        s ~ '[], t ~ '[], a ~ (), b ~ (),
        r ~ Proxy, p ~ (->))

type LabeledCxt x r to p f s t a b = (LabeledCxt1 x r to p f s t a b,
                                      Labelable x r to p f s t a b)

-- | make a @Prism (Variant s) (Variant t) a b@
instance (HPrism x p f s t a b,
          to ~ (->)) => Labelable x Variant to p f s t a b where
    hLens' x s = hPrism x s


-- | make a @Lens' (TIP s) a@.
--
-- 'tipyLens' provides a @Lens (TIP s) (TIP t) a b@, which tends to need
-- too many type annotations to be practical
instance (s ~ t, a ~ b, x ~ a,

      HUpdateAtLabel TIP x b s t,
      HUpdateAtLabel TIP x a t s,
      SameLength s t,

      Functor f,
      (->) ~ to,
      (->) ~ p) =>
    Labelable x TIP to p f s t a b where
    hLens' x f s = fmap (\b -> hUpdateAtLabel x b s) (f (s .!. x))


-- | modification of '.=.' which works with the labels from this module,
-- and those from "Data.HList.Label6". Note that this is not strictly a
-- generalization of '.=.', since it does not work with labels like
-- "Data.HList.Label3" which have the wrong kind.
l .==. v = toLabel l .=. v

infixr 4 .==.


-- | extracts the type that is actually the label in @a@ and puts it in @b@
class ToSym (a :: *) (b :: k) | a -> b

-- | for labels in this module
instance XFromLabeled v1 v2 v3 x => ToSym (v1 v2 v3) (x :: Symbol)


-- | extracts the label from a LabeledOptic ... ~ v1 v2 v3
class XFromLabeled v1 v2 v3 x | v1 v2 v3 -> x

instance (LabeledCxt1 x r (Labeled x) p f s t a b,
          (v1 v2 v3) ~ LabeledOptic (Labeled x) p f (r s) (r t) a b) =>
    XFromLabeled v1 v2 v3 x

-- | for "Data.HList.Label6" labels
instance (x ~ x') => ToSym (Label x) x'


toLabel :: ToSym t t' => t -> Label (t' :: Symbol)
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
