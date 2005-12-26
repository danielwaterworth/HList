{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- Needed for a reply to the Haskell mailing list

import CommonMain

main = print $ comp "abc"

test = HCons (length::String -> Int) (HCons ((+1)::(Int->Int)) (HCons ((*2)::(Int->Int)) HNil))

data Comp

instance Apply Comp (x -> y,y -> z) (x -> z)
 where
  apply _ (f,g) = g . f

-- Function composition based on type code works.

comp  = hFoldr (undefined::Comp) (id::Int -> Int) test

-- Function composition based on normal polymorphism doesn't
-- comp' = hFoldr (uncurry (flip (.))) (id::Int -> Int) test

