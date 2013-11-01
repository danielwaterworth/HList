{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{- | Description : quasiquoter inspired by -XNamedFieldPuns -}
module Data.HList.RecordPuns (
    -- $ex
    pun

    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.HList.Record
import Data.HList.FakePrelude
import Data.List


{- $ex

>>> :set -XQuasiQuotes -XViewPatterns

[@patterns@]

>>> let y = Label :: Label "y"
>>> let x = Label :: Label "x"
>>> [pun| x y |] <- return (x .=. 3 .*. y .=. "hi" .*. emptyRecord)
>>> print (x,y)
(3,"hi")

[@expressions@]

Compare with the standard way to construct records above

>>> let x = 3; y = "hi"
>>> [pun|x y|]
Record{x=3,y="hi"}

[@nesting@]

Nesting is supported. The idea is that variables inside
@{ }@ are in another record. More concretely:

> [pun| ab@{ a b } y z c{d} |]

as a pattern, it will bindings from an original record @x@,
if you interpret (.) as a left-associative field lookup (as it
is in other languages):

> let ab = xab
>     a = x.ab.a
>     b = x.ab.b
>     y = x.y
>     z = x.z
>     -- c is not bound
>     d = x.c.d

as an expression, it creates a new record which needs the variables
@ab a b y z d@ in-scope. @ab@ needs to be a record, and if it has
fields called @a@ or @b@ they are overridden by the values of @a@ and @b@
which are in scope.

See also @examples/pun.hs@.
-}


-- | requires the use of "Data.HList.Label6" (ie. the label for foo is @Label :: Label \"foo\"@)
pun :: QuasiQuoter
pun = QuasiQuoter {
    quotePat = mp . parseRec,
    quoteExp = me . parseRec,
    quoteDec  = error "Data.HList.RecordPuns.quoteDec",
    quoteType = error "Data.HList.RecordPuns.quoteType"
 }


-- like  \x -> (x .!. x1, x .!. x2)
extracts xs = do
    record <- newName "record"
    lamE [varP record]
        (tupE
            [ [| $(varE record) .!. $label  |]
                | x <- xs,
                let label = [| Label :: Label $(litT (strTyLit x)) |]
                ])

mkExp :: [String] -> ExpQ
mkExp xs = foldr (\x acc -> [| $(mkPair x (dyn x)) .*. $acc |]) [| emptyRecord |] xs

mkPair :: String -> ExpQ -> ExpQ
mkPair x xe = [| (Label :: Label $(litT (strTyLit x))) .=. $xe |]



me :: Tree -> ExpQ
me (C as) = foldr (\(l,e) acc -> [| $(mkPair l e) .*. $acc |]) [| emptyRecord |] (mes as)
me a = do
    reportWarning $ "Data.HList.RecordPuns.mp implicit {} added around:" ++ show a
    me (C [a])

mes :: [Tree] -> [(String, ExpQ)]
mes (V a : V "@": b : c) = (a, [| $(me b) `hLeftUnion` $(dyn a) |]) : mes c
mes (V a : C b : c)      = (a, me (C b)) : mes c
mes (V a : b)            = (a, varE (mkName a)) : mes b
mes [] = []
mes inp = error $ "Data.HList.RecordPuns.mes: cannot translate remaining:" ++
                        show (map ppTree inp)

mp :: Tree -> PatQ
mp (C as) = case unzip (mps as) of
    (a, b) -> viewP (extracts a) (tupP b)
mp a = do
    reportWarning $ "Data.HList.RecordPuns.mp implicit {} added around:" ++ show a
    mp (C [a])

mps :: [Tree] -> [(String, PatQ)]
mps (V a : V "@" : b : c) = (a, asP (mkName a) (mp b)) :  mps c
mps (V a : C b : c) = (a, mp (C b)) : mps c
mps (V a : b) = (a, varP (mkName a)) : mps b
mps [] = []
mps inp = error $ "Data.HList.RecordPuns.mps: cannot translate remaining pattern:" ++
                        show (map ppTree inp)


data Tree = C [Tree] | V String deriving Show

{- |

>>> parseRec "{ a b c {d e f}  } d"
C [C [V "a",V "b",V "c",C [V "d",V "e",V "f"]],V "d"]

>>> ppTree $ parseRec "{a b c {d e {} f @ g}}"
"{a b c {d e {} f @ g}}"

> ppTree $ parseRec "a b c {d e {} f @ g}"
"{a b c {d e {} f @ g}}"

-}
parseRec :: String -> Tree
parseRec str = case parseRec' 0 [[]] $ lexing str of
    [x] -> x -- avoid adding another layer if possible
    x -> C (reverse x)

parseRec' :: Int -> [[Tree]] -> [String] -> [Tree]
parseRec' n accum  ("{" : rest)  = parseRec' (n+1) ([] : accum) rest
parseRec' n (a:b:c) ("}" : rest) = parseRec' (n-1) ((C (reverse a) : b) : c)  rest
parseRec' n (b:c) (a   : rest)
         | a `notElem` ["{","}"] = parseRec' n     ((V a : b) : c) rest
parseRec' 0 (a:_) []             = a
parseRec' _ accum e              = error ("Data.HList.RecordPuns.parseRec' unexpected: " ++ show e
                                            ++ "\n parsed:" ++ show (reverse accum))

ppTree :: Tree -> String
ppTree (C ts) = "{" ++ unwords (map ppTree ts) ++ "}"
ppTree (V x)  = x

lexing = unfoldr (\v -> case lex v of
                    ("", "") : _ -> Nothing
                    e : _ -> Just e
                    _ -> Nothing)
