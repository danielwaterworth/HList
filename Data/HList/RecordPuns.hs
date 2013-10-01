{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
module Data.HList.RecordPuns (
    -- $ex
    pun
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.HList.Record
import Data.HList.FakePrelude


{- $ex

>>> let y = Label :: Label "y"
>>> let x = Label :: Label "x"
>>> [pun| x y |] <- return (x .=. 3 .*. y .=. "hi" .*. emptyRecord)
>>> print (x,y)
(3,"hi")

-}


-- | requires the use of "Data.HList.Label6" (ie. the label for foo is @Label :: Label \"foo\"@)
pun :: QuasiQuoter
pun = QuasiQuoter {
    quotePat = \str -> mkPat (words str),
    quoteExp  = error "Data.HList.RecordPuns.quoteExp",
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

