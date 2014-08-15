{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- more examples for record puns
module Main where
import Data.HList.CommonMain

makeLabels6 (words "a b c")


r  = c .=. "c" .*. b .=. (a .=. 3 .*. emptyRecord) .*. emptyRecord
r2 = b .=. (a .=. 1 .*. emptyRecord) .*. emptyRecord


p1 ( (.!. b) -> (b @ ((.!. a) -> a))) = (a,b)

p2 [pun| b @ {a} |] = (a, b)

-- same as p2, but gives a warning
-- p3 [pun| b @ a |] = (a, b)

p4 [pun| b{a} |] = a -- b is not bound

-- adds `x' and `y' into a field called r
e1 = let x = 1; y = "hi" in [pun| r @ { x y } |]

-- updates the `c' field
e2 = let c = 1; y = "hi" in [pun| r @ { c y } |]

-- same as e1, but doesn't use a pre-existing r
e3 = let x = 1; y = "hi" in [pun| r { x y } |]


main = do
        putStrLn "similar:"
        print $ p1 r
        print $ p2 r
        print $ p4 r

        putStrLn "\nexpression QQ:"
        print $ e1
        print $ e2
        print $ e3

