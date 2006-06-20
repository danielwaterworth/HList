{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- Modeling of an extensible sum datatype
--    data List a = Nil | Cons a (List a)
-- which is later extended with two more variants,
--    Unit a | App (List a) (List a)
-- 
-- Our goals:
--   - any function that accepts that accepts the extended data list
--     must also accept the unextended list.
--   - it should be possible to `extend' the function that operates
--     on plain lists to operate on extended list. We should be able
--     to reuse as much of the old code as possible.
-- It seems, we have achieved our goals.

-- Method: duality rules! Curry-Howard correspondence and logic 
-- give the wonderful guidance.

-- Note, the code below has no type classes, no type-level programming
-- or any other type hacking. In fact, there are no type annotations,
-- type declarations, or any other mentioning of types, except in the comments

module VariantP where

import CommonMain
import GhcSyntax
import Label1


-- Declare labels for extensible records -- or, dually, for
-- extensible variants
-- We used the simplest field labels. They are a bit
-- ungainly to use -- but let us avoid overlapping instance extension.
-- See OOHaskell for the use of more advanced labels.

l_nil   = firstLabel
l_cons  = nextLabel l_nil

nil consumer = consumer .!. l_nil

cons a l consumer = (consumer .!. l_cons) a (l consumer)

tl1 c = cons 'a' (cons 'b' (cons 'c' nil)) c
tl2 c = cons 10 (cons 1 (cons 2 (cons 3 nil))) c

{-
  The inferred type of tl1 is

*Variants> :t tl1
tl1 :: (HasField (Label (HSucc HZero)) r (Char -> v -> v),
	HasField (Label HZero) r v) =>
       r -> v

which basically says that tl1 accepts any consumer that has at least
fields l_nil and l_cons with appropriate types. The consumer
may have more fields
-}


-- First polymorphic function: provide a regular list "view" of our list

to_list () =     l_nil  .=. []
	     .*. l_cons .=. (:) 
	     .*. emptyRecord

-- I wish GHC supported unicode in identifiers
tekiyou consumer lst = lst (consumer ())

test1 = tekiyou to_list tl1
test2 = tekiyou to_list tl2

-- another function, length

lengthL () =  l_nil  .=. 0
	  .*. l_cons .=. (\x a -> a + 1)
	  .*. emptyRecord

test_lL1 = tekiyou lengthL tl1
test_lL2 = tekiyou lengthL tl2


-- Now, add extension to our record -- and, dually, extend our variant

l_unit = nextLabel l_cons
l_app  = nextLabel l_unit

unit a c = (c .!. l_unit) a

app l1 l2 c = (c .!. l_app) (l1 c) (l2 c)

tl3 c = cons 1 (unit 2) c
tl4 c = cons 10 (app tl3 tl2) c

sumA () =  l_nil  .=. 0
       .*. l_cons .=. (+)
       .*. l_unit .=. id
       .*. l_app  .=. (+)
       .*. emptyRecord

-- we can apply sum to an original (unextended) list
test_sum1 = tekiyou sumA tl2

-- now, we can apply sum to an extended list
test_sum2 = tekiyou sumA tl4


-- we can't pass extended lists tl3 and tl4 to a regular lenghtL
-- The error message says that the field l_unit is missing
-- test_lL3 = tekiyou lengthL tl3


-- now we can attempt to extend lengthL, reusing as much of previous
-- functionality as possible

lengthA () =  l_unit .=. const 1
	  .*. l_app  .=. (+)
	  .*. (lengthL ())

test_lL4 = tekiyou lengthA tl3
test_lL5 = tekiyou lengthA tl4


-- Binary methods.
-- Hmm, doing it directly is complicated. OTH, we can use the list view
-- (to_list) and solve the problem this way...
-- Laziness helps...

eqL l1 l2 = tekiyou to_list l1 == tekiyou to_list l2
test_eq1 = eqL tl1 tl1
test_eq2 = eqL tl1 (cons 'a' nil)
test_eq3 = eqL tl1 nil

-- we can extend to_list as usual

to_listA () =  l_unit .=. (:[])
	   .*. l_app  .=. (++)
	   .*. (to_list ())

-- And we can arbitrary mix old and extended lists
eqA l1 l2 = tekiyou to_listA l1 == tekiyou to_listA l2
test_eq4 = eqA tl1 tl1
test_eq5 = eqA tl2 tl3
test_eq6 = eqA tl3 tl4
