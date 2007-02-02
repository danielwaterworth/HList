{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

data Foo x y
class Bar x y | x -> y
class Zoo x y | x -> y

{-

Works for both GHC and Hugs

instance Bar (Foo x y) y
instance Bar (Foo (Foo x y) z) z

-}

{-

Works for GHC but not Hugs

-}

instance Zoo x r => Bar (Foo x y) r
instance Zoo x r => Bar (Foo (Foo x y) z) r


