{- |

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-main module that loads all modules that at least
   *compile* fine for all the models of interest. See the Makefile
   for ways to run different models.

-}

module Data.HList.CommonMain (

   module Data.HList.FakePrelude
 , module Data.HList.HListPrelude
 , module Data.HList.HArray
 , module Data.HList.HOccurs
 , module Data.HList.HTypeIndexed
 , module Data.HList.Record
 , module Data.HList.HList
 , module Data.HList.TypeEqO
 , module Data.HList.TIP
 , module Data.HList.TIC
 , module Data.HList.HZip
 , module Data.HList.Variant

 -- * "Data.HList.Keyword"
 -- | the \"public\" parts. More examples are in the module documentation.
 , Kw(..), recToKW, IsKeyFN, K,  ErrReqdArgNotFound,  ErrUnexpectedKW

 -- * Labels
 -- | there are three options for now:

 , module Data.HList.Label3

 -- $label6demo

 -- | template haskell for automating different types of labels
 , module Data.HList.MakeLabels
 -- | quasiquoter to avoid those labels in the first place
 , module Data.HList.RecordPuns


 -- $instancesFrom
 -- These modules provide useful instances:
 --
 -- * "Data.HList.Data"
) where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HArray
import Data.HList.HOccurs
import Data.HList.HTypeIndexed
import Data.HList.Record
import Data.HList.HList
import Data.HList.MakeLabels
import Data.HList.TypeEqO
import Data.HList.TIP
import Data.HList.TIC

import Data.HList.HZip
import Data.HList.Label3
import Data.HList.Label6 () -- only instances

import Data.HList.Variant

import Data.HList.Data () -- only instances

import Data.HList.Keyword
import Data.HList.RecordPuns

{- $label6demo #label6demo#

 Instances from "Data.HList.Label6"

>>> :set -XDataKinds
>>> (Label :: Label "x") .=. (5::Int) .*. emptyRecord
Record{x=5}

>>> let x = Label :: Label "x"
>>> let r = x .=. (5::Int) .*. emptyRecord
>>> r .!. x
5

-}


