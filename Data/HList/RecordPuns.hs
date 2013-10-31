{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{- | Description : quasiquoter emulating -XNamedFieldPuns -}
module Data.HList.RecordPuns (
    -- $ex
    pun
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.HList.Record
import Data.HList.FakePrelude


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

-}


-- | requires the use of "Data.HList.Label6" (ie. the label for foo is @Label :: Label \"foo\"@)
pun :: QuasiQuoter
pun = QuasiQuoter {
    quotePat = \str -> mkPat (words str),
    quoteExp  = \str -> mkExp (words str),
    quoteDec  = error "Data.HList.RecordPuns.quoteDec",
    quoteType = error "Data.HList.RecordPuns.quoteType"
 }

mkPat :: [String] -> PatQ
mkPat xs = viewP extracts binds
  where

  -- like (x1,x2,x3)
  binds = tupP (map (varP . mkName) xs)

  -- like  \x -> (x .!. x1, x .!. x2)
  extracts = do
    record <- newName "record"
    lamE [varP record]
        (tupE
            [ [| $(varE record) .!. $label  |]
                | x <- xs,
                let label = [| Label :: Label $(litT (strTyLit x)) |]
                ])

mkExp :: [String] -> ExpQ
mkExp xs = foldr (\x acc -> [| $(mkPair x) .*. $acc |]) [| emptyRecord |] xs
  where
  mkPair :: String -> ExpQ
  mkPair x = [| (Label :: Label $(litT (strTyLit x))) .=. $(dyn x) |]
