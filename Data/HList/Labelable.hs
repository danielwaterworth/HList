{-# LANGUAGE TemplateHaskell #-}
{- |

Description : labels which are also lenses

A simple problem is being solved here, but unfortunately it
is a bit involved. The idea is to use the same haskell identifier 
for a lens and for other purposes. In other words, get the same
behavior as:

 > x = hLens (Label :: Label "x")
 > r ^. x

While still being able to extract the symbol \"x\" from x, so that things
like @x .=. 123@ could be acceptable. In this case we don't overload '.=.',
so instead you have to write @x .==. 123@.

Note that the original hLens is still useful, since the following
needs to apply the @x@ for different @Functor f =>@, so you would
have to write a type signature (rank-2)

 > let f x r = let
 >          a = r ^. x
 >          b = r & x .~ "6"
 >        in (a,b)

The alternative that calls 'hLens' would not need any type signature,
since each time you write @hLens x@, you can get a different type
even if the @x :: Label \"x\"@ is a monomorphic type.

Elaboration of some ideas from edwardk.
-}
module Data.HList.Labelable
    (makeLabelable,
     Labelable(hLens'),
     (.==.),

    -- * likely unneeded (re)exports
    -- $note needed to make a needed instance visible
    Labeled(Labeled),
    toLabel,
    Identity,
    ToSym,
    ) where


import Data.HList.FakePrelude
import Data.HList.HArray
import Data.HList.HList
import Data.HList.Record

import Control.Monad.Identity
import GHC.TypeLits

import Language.Haskell.TH

{- | @s t a b@ type parameters are the same as those that make
"Control.Lens" work.

[@n@] is the index in the HList at which the value will be found

[@l@] is the label for the field (tends to be "GHC.TypeLits.Symbol")

[@p@] is @->@ when the result is used as a lens, or 'Labeled' when used
      as an argument to '.==.'

-}
class Labelable (n :: HNat) l p f s t a b | l s -> a, l t -> b, l s b -> t, l t a -> s, l s -> n, l t -> n where
    hLens' :: Label l -> p (a -> f b) (Record s -> f (Record t))

data Labeled (l :: k) (a :: *) (b :: *) = Labeled deriving (Show)

-- | make a lens
instance (Functor f,
          HasField x (Record s) a,
          HasField x (Record t) b,
          HFind x (RecordLabels t) n,
          HFind x (RecordLabels s) n,
          HUpdateAtHNat n (LVPair x b) s,
          t ~ HUpdateAtHNatR n (LVPair x b) s)
        => Labelable n x (->) f s t a b where
            hLens' lab f rec = fmap (\v -> hUpdateAtLabel lab v rec) (f (rec .!. lab))

-- | make a data type that allows recovering the field name
instance (f ~ Identity, n ~ HZero, s ~ '[], t ~ '[], a ~ (), b ~ (),
           x' ~ x) => Labelable n x' (Labeled x) f s t a b where
        hLens' _ = Labeled :: Labeled x (a -> f b) (Record s -> f (Record t))


-- | modification of '.=.' which works with the labels from this module,
-- and those from "Data.HList.Label6". Note that this is not strictly a
-- generalization of '.=.', since it does not work with labels like
-- "Data.HList.Label3" which have the wrong kind.
l .==. v = toLabel l .=. v


-- | extracts the type that is actually the label in @a@ and puts it in @b@
class ToSym a b

-- | for labels in this module
instance (x ~ x', p ~ Labeled x') => ToSym (p a b) x'

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
            where lt = [| Label :: $([t| Label $(litT (strTyLit x)) |]) |]

        makeSig = [t| Labelable n l p f s t a b => p (a -> f b) (Record s -> f (Record t)) |]
