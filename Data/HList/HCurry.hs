{- |

 Description : curry / uncurry

 Convert between functions taking HLists and functions taking many arguments

-}
module Data.HList.HCurry where

import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.TypeEqO () -- Arity instance

type HCurry f xs r = HCurry' (HLength xs) f xs r

-- | Ideally this would just be @HLength xs ~ n@. However,
-- the second constraint allows refining xs to '[a, b, c]
-- if n is known to be 3.
type HLengthEq xs n =
       (HLength xs ~ n,
        SameLength' (HReplicateR n ()) xs)



{- | 'curry'/'uncurry' for many arguments and HLists instead of tuples

-}
class (HLengthEq xs n) => HCurry' (n :: HNat) f xs r
          | f xs -> r, r xs -> f, n f -> xs where
    hUncurry' :: Proxy n -> f -> HList xs -> r
    hCurry' :: Proxy n -> (HList xs -> r) -> f

instance HCurry' HZero b '[] b where
    hUncurry' _ b _ = b
    hCurry' _ f = f HNil

instance (HCurry' n b xs r) => HCurry' (HSucc n) (x -> b) (x ': xs) r where
    hUncurry' n f (HCons x xs) = hUncurry' (hPred n) (f x) xs
    hCurry' n f x = hCurry' (hPred n) (f . HCons x)

hUncurry f = hUncurry' (arityOf f) f

hCurry f = let f' = hCurry' (arityOf f') f
           in f'

{- | compose two functions that take multiple arguments. The result of the
second function is the first argument to the first function. An example is
probably clearer:

>>> let f = hCompose (,,) (,)
>>> :t f
f :: x -> x1 -> x2 -> x3 -> ((x, x1), x2, x3)

>>> f 1 2 3 4
((1,2),3,4)

Note: polymorphism can make it confusing as to how many parameters a function
actually takes. For example, the first two ids are @id :: (a -> b) -> (a -> b)@ in

>>> (.) id id id 'y'
'y'

>>> hCompose id id id 'y'
'y'

still typechecks, but in that case @hCompose i1 i2 i3 x == i1 ((i2 i3) x)@
has id with different types than @(.) i1 i2 i3 x == (i1 (i2 i3)) x

Prompted by <http://stackoverflow.com/questions/28932054/can-hlistelim-be-composed-with-another-function>

-}
hCompose f g = hCurry $ \xs -> case hSplitAt Proxy xs of
        (xg,xf) -> hUncurry f (hUncurry g xg `HCons` xf)


arityOf :: Arity f n => f -> Proxy n
arityOf _ = Proxy
