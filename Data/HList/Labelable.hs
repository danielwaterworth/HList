{-# LANGUAGE TemplateHaskell #-}
{- | A simple problem is being solved here, but unfortunately it
is a bit involved. The idea is to use the same haskell identifier 
for a lens and for other purposes. In other words, get the same
behavior as:

 > x = hLens (Label :: Label "x")
 > r ^. x

While still being able to have @x .=. 123@.

Elaboration of some ideas from edwardk.

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

-}
module Data.HList.Labelable
    (makeLabelable,
     Labelable,
     Labeled(Labeled),
     (.==.),
     toLabel,
    ) where


import Data.HList.FakePrelude
import Data.HList.HArray
import Data.HList.HList
import Data.HList.Record

import Control.Monad.Identity
import GHC.TypeLits

import Language.Haskell.TH

class Labelable (n :: HNat) l p f s t a b | l s -> a, l t -> b, l s b -> t, l t a -> s, l s -> n, l t -> n where
    label :: Label l -> p (a -> f b) (Record s -> f (Record t))

data Labeled (l :: k) (a :: *) (b :: *) = Labeled deriving (Show)

instance (Functor f,
          HasField x (Record s) a,
          HasField x (Record t) b,
          HFind x (RecordLabels t) n,
          HFind x (RecordLabels s) n,
          HUpdateAtHNat n (LVPair x b) s,
          t ~ HUpdateAtHNatR n (LVPair x b) s)
        => Labelable n x (->) f s t a b where
            label lab f rec = fmap (\v -> hUpdateAtLabel lab v rec) (f (rec .!. lab))

instance (f ~ Identity, n ~ HZero, s ~ '[], t ~ '[], a ~ (), b ~ (),
           x' ~ x) => Labelable n x' (Labeled x) f s t a b where
        label _ = Labeled :: Labeled x (a -> f b) (Record s -> f (Record t))


-- | modification of '.=.' which works with the labels from this module,
-- and those from "Data.HList.Label6". Note that this is not strictly a
-- generalization of '.=.', since it does not work with labels like Label3
l .==. v = toLabel l .=. v


type family ToSym a :: k
type instance ToSym (Label x) = x
type instance ToSym (Labeled x a b) = x

toLabel :: t -> Label (ToSym t :: Symbol)
toLabel _ = Label

{- ^

>>> :set -XNoMonomorphismRestriction -XDataKinds -XPolyKinds
>>> import Control.Lens
>>> let x = label (Label :: Label "x")

The original way:

>>> let r = (Label :: Label "x") .=. "5" .*. emptyRecord

The improved way:

>>> let r2 = x .==. "5" .*. emptyRecord

>>> r ^. x
"5"

>>> r2 ^. x
"5"

>>> r & x .~ ()
Record{x=()}

-}


{- | @makeLabelable \"x y z\"@ will generate haskell identifiers that work with '.==.' and
are also lenses.

> x = label (Label :: Label "x")
> y = label (Label :: Label "y")
> z = label (Label :: Label "z")

-}
makeLabelable :: String -> Q [Dec]
makeLabelable xs = fmap concat $ mapM makeLabel1 (words xs)
    where
        -- a bit indirect, ghc-7.6 TH is a bit too eager to reject
        -- mis-matched kind variables
        makeLabel1 x = sequence
              [
                sigD (mkName x) makeSig,
                valD (varP (mkName x)) (normalB (varE 'label `appE` lt))
                            []
                ]
            where lt = [| Label :: $([t| Label $(litT (strTyLit x)) |]) |]

        makeSig = [t| Labelable n l p f s t a b => p (a -> f b) (Record s -> f (Record t)) |]
