{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
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
    (makeLabelable,
     Labelable(hLens'),
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


-- | modification of '.=.' which works with the labels from this module,
-- and those from "Data.HList.Label6". Note that this is not strictly a
-- generalization of '.=.', since it does not work with labels like
-- "Data.HList.Label3" which have the wrong kind.
l .==. v = toLabel l .=. v

infixr 4 .==.


-- | extracts the type that is actually the label in @a@ and puts it in @b@
class ToSym a b

-- | for labels in this module
instance (LabeledCxt1 x r (Labeled x) p f s t a b,
          (v1 v2 v3) ~ LabeledOptic (Labeled x) p f (r s) (r t) a b)
  => ToSym (v1 v2 v3) x

-- | for "Data.HList.Label6" labels
instance (x ~ x') => ToSym (Label x) x'

toLabel :: ToSym t t' => t -> Label (t' :: Symbol)
toLabel _ = Label



{- | @makeLabelable \"x y z\"@ will generate haskell identifiers that work with '.==.' and
are also lenses.

> x = hLens' (Label :: Label "x")
> y = hLens' (Label :: Label "y")
> z = hLens' (Label :: Label "z")

-}
makeLabelable :: String -> Q [Dec]
makeLabelable xs = fmap concat $ mapM makeLabel1 (words xs)
    where
        -- a bit indirect, ghc-7.6 TH is a bit too eager to reject
        -- mis-matched kind variables
        makeLabel1 x = sequence
              [
                sigD (mkName x) makeSig,
                valD (varP (mkName x)) (normalB (varE 'hLens' `appE` lt))
                            []
                ]
            where lt = [| Label :: $([t| Label $l |]) |]
                  l = litT (strTyLit x)

                  makeSig = [t| (Labelable $l r to p f s t a b) =>
                              -- (a `p` f b) `to` (r s `p` f (r t))
                              LabeledOptic to p f (r s) (r t) a b
                              |]


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
