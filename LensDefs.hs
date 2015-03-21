-- | parts of lens that would be imported if we depended on it
module LensDefs
  (module LensDefs,
   module Control.Applicative,
   Choice,
   Profunctor)
   where

import Data.Profunctor
import Data.Profunctor.Unsafe
import Control.Applicative
import Control.Monad.Identity

simple :: p a (f a) -> p a (f a)
simple = id


-- Used by doctests (which should probably just import Control.Lens...)
infixl 1 &
x & f = f x

infixr 4 %~
l %~ f = \t -> runIdentity $ l (rmap Identity f) t

iso :: (Profunctor p, Functor f)
    => (s -> a) -> (b -> t)
    -> p a (f b) -> p s (f t)
iso sa bt = dimap sa (fmap bt)

-- | iso, except assumes that the functions supplied could
-- be 'Data.Coerce.coerce'
isoNewtype :: (Profunctor p, Functor f)
    => (s -> a) -> (b -> t)
    -> p a (f b) -> p s (f t)
isoNewtype sa bt x = fmap bt #. x .# sa

prism :: (b -> t) -> (s -> Either t a)
    -> (forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t))
prism bt seta = dimap seta (either pure (fmap bt)) . right'

prism' :: (a -> s) -> (s -> Maybe a)
    -> (forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s))
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
