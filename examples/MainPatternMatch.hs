{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Pattern-matching on HList's Records

{-
  See the thread `Re: (small) records proposal for Haskell '06'
  Haskell mailing list, January 2006
  http://www.haskell.org/pipermail/haskell/2006-January/017276.html

Joel Reymont wrote:
> How does pattern matching work with HList?
> I would like to pass a HList to a function and only match if a
> certain field had a certain value.

The code below defines the function foo that accepts a record and
yields one value if the field PtX of the record has the value 0. If
the field has any other value, a different result is returned. The
function is written in a pattern-matching style. Also, the function is
record-polymorphic: it takes _any_ record (of any `record type') that
happens to have the field names PtX.

        *Test> :t foo
        foo :: (Num v, HasField (Proxy PtX) r v) => r -> [Char]

-}

module Main where

import Data.HList.CommonMain
import Data.HList.GhcSyntax

import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1
import Data.HList.Label4

-- Labels
-- The more convenient labels, Label4.hs, need -fallow-overlapping-instances
-- The less convenient label representation needs fewer extensions.
-- We go for more convenient...

data PtX; px = proxy::Proxy PtX
data PtY; py = proxy::Proxy PtY

infixr 9 #
m # field = m .!. field

accessor r f = r # f

-- 1D points
point1 x = 
       px .=. x
   .*. emptyRecord

-- 2D points
point2 x y = 
       px .=. x
   .*. py .=. (y + 10)
   .*. emptyRecord

-- Record-polymorphic function, which illustrates record pattern-matching,
-- with the help of generalized guards
foo p | 0 <- p # px = "X is zero"
foo _ = "something else"

test1  = foo (point1 0)
test1' = foo (point1 42)
test2 = foo (point2 10 20)
-- inline construction of the record
test3 = foo (py .=. False .*. px .=. 0 .*. emptyRecord)

main = do
       putStrLn test1
       putStrLn test1'
       putStrLn test2
       putStrLn test3
