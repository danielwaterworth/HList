
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

   (There are other ways: see ConsUnion.hs, for example)
-}

module Data.HList.TIC where

import Data.Dynamic

import Data.HList.HList
import Data.HList.TIP
import Data.HList.HTypeIndexed ()
import Data.HList.HListPrelude
import Data.HList.HArray



import Data.Profunctor
import Control.Applicative

-- --------------------------------------------------------------------------
-- | A datatype for type-indexed co-products

newtype TIC (l :: [*]) = TIC Dynamic


-- --------------------------------------------------------------------------
-- | Public constructor (or, open union's injection function)

mkTIC' :: ( HTypeIndexed l
         , HMember i l True
         , Typeable i
         )
      => i
      -> Proxy l -- ^ the ordering of types in the @l :: [*]@ matters.
                 -- This argument is intended to fix the ordering 
      -> TIC l

mkTIC' i _ = TIC (toDyn i)

mkTIC i = mkTIC' i Proxy


-- --------------------------------------------------------------------------
-- | Public destructor (or, open union's projection function)

unTIC :: ( HTypeIndexed l
         , HMember o l True
         , Typeable o
         )
      => TIC l -> Maybe o

unTIC (TIC i) = fromDynamic i

-- | @Prism (TIC s) (TIC t) a b@
ticPrism
  :: (Choice p,
      Applicative f,

      HTypeIndexed s,
      HTypeIndexed t,

      HType2HNat a s n,
      HType2HNat b t n,

      HUpdateAtHNatR n b s ~ t,
      HUpdateAtHNatR n a t ~ s,

      HMember a s True,
      HMember b t True,

      Typeable a,
      Typeable b) =>
     p a (f b) -> p (TIC s) (f (TIC t))
ticPrism = prism mkTIC (\s @ (TIC s') -> case unTIC s of
                       Just a -> Right a
                       Nothing -> Left (TIC s'))
  where
    prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- --------------------------------------------------------------------------
-- | TICs are opaque

instance Show (TIC l)
 where
  show _ = "<Cannot show TIC content!>"

