{- |

   Description : import me
   Copyright   : (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   The HList library

   This module re-exports everything needed to use HList.

-}

module Data.HList.CommonMain (

 -- * Faking dependent types in Haskell
   module Data.HList.FakePrelude

 -- * Functions for all collections
 , module Data.HList.HListPrelude
 -- * Array-like access to HLists
 , module Data.HList.HArray
 -- * Result-type-driven operations
 , module Data.HList.HOccurs
 -- * Type-indexed operations
 , module Data.HList.HTypeIndexed

 -- * Record
 , module Data.HList.Record
 -- | quasiquoter 'pun' helps to avoid needing a proxy value with
 -- type 'Label' in the first place: when you take values out of or into
 -- records with pattern matching, the variable name determines the label
 -- name.
 , module Data.HList.RecordPuns


 -- * HList
 -- | A subset of "Data.HList.HList" is re-exported.
 , module Data.HList.HList
 , module Data.HList.HZip
 -- ** A subset of "Data.HList.HSort"
 , hSort
 , HSortBy(..)
 , HLeFn, HDown
 , HSet, HSetBy
 , HIsSet, HIsSetBy
 , HAscList, HIsAscList

 -- ** A subset of "Data.HList.HCurry"
 , HCurry
 , HCurry'(..)
 , hCurry, hUncurry
 , hCompose

 -- * TIP
 -- | Public interface of "Data.HList.TIP"
 , TIP
 , emptyTIP
 , tipyUpdate
 , tipyLens
 , tipyLens'
 -- ** projection
 , tipyProject
 , tipyProject2
 , tipyTuple
 , tipyTuple3
 , tipyTuple4
 , tipyTuple5
 , TagUntag, TagUntagFD(..)
 , TagR

 -- ** TIP transform
 , TransTIP(..)
 , TransTIPM(..)

 -- * TIC
 -- | Public interface of "Data.HList.TIC"
 , TIC
 -- ** creating TIC
 , mkTIC
 , mkTIC1
 , mkTIC'

 -- ** get,set,modify
 , ticPrism, ticPrism'

 -- * Variant
 -- | Public interface of "Data.HList.Variant"
 , Variant
 , mkVariant
 , mkVariant1
 , castVariant
 , HMapV(..), hMapV
 , hMapOutV
 , ZipVariant(..)
 , ZipVR(..), zipVR
 -- ** projection
 -- *** many
 , SplitVariant(splitVariant)
 , ProjectVariant(..)
 , ExtendsVariant(..)
 , ProjectExtendVariant(..)
 -- *** one
 , HPrism(..)
 , unvarianted, unvarianted'

 , splitVariant1
 , splitVariant1'
 , extendVariant
 -- **** implementation
 , Unvariant(..)
 , Unvariant'(..)



 -- * Conversions between collections
 -- $convention the foo' optic has the same type as
 -- @Control.Lens.simple . foo@. 'hLens'' is an exception to this rule.

 , TypeIndexed(..)
 , typeIndexed'
 -- ** HList and Record
 -- | 'unlabeled' 'unlabeled''

 -- ** HList and TIP
 , tipHList, tipHList'

 -- ** Newtype wrappers
 -- $convention these isos unwrap/wrap the newtypes 'TIP' 'TIC' and
 -- 'Record'. Names follow the pattern @fromTo :: Iso' From To@.

 -- | 'hListRecord' 'hListRecord'' are exported under "Data.HList.Record"
 , ticVariant, ticVariant'
 , tipRecord, tipRecord'

 -- ** Record and Variant
 , hMaybied, hMaybied'
 -- *** implementation
 , VariantToHMaybied(variantToHMaybied)
 , HMaybiedToVariantFs
 , hMaybiedToVariants

 -- * "Data.HList.Keyword"
 -- | the \"public\" parts. More examples are in the module documentation.
 , Kw(..), recToKW, IsKeyFN, K,  ErrReqdArgNotFound,  ErrUnexpectedKW

 -- * Labels
 {- | there are three options for now. However, there are
   a couple different styles for the first option here:

   GHC supports type-level strings ('GHC.TypeLits.Symbol'), and these can be
   labels. You can refer to these strings using an unwieldy syntax described
   below. For example if you want to store a value @5@ in a record @rec@
   with a field called @\"x\"@, and then get it out again:

   let rec = ('Label' :: Label \"x\") '.=.' 5 '.*.' 'emptyRecord'

   rec '.!.' (Label :: Label \"x\")

   To avoid that pain, you can have a definition @x = Label :: Label "x"@.
   and just use @x@ instead of repeating @Label :: Label \"x\"@ so that
   a lookup becomes:

   > rec .!. x

   'makeLabels6' automates definitions like @x = Label :: Label \"x\"@.

 -}
 -- $label6demo

 , module Data.HList.Labelable
 -- $labelable

 -- ** namespaced labels
 , module Data.HList.Label3

 -- ** labels as any instance of Typeable
 --  | "Data.HList.Label5"

 -- ** template haskell
 , module Data.HList.MakeLabels


 -- * "Data.HList.Data"
 -- | This modules provide useful instances. A useful application can be
 -- found in @examples/cmdargs.hs@

 -- | Overlapping instances are restricted to here
 , module Data.HList.TypeEqO

 -- * Internals
 -- | internals exported for type signature purposes
 , HAllTaggedEq
) where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HArray
import Data.HList.HOccurs
import Data.HList.HTypeIndexed
import Data.HList.Record
-- import Data.HList.RecordU
-- import Data.HList.RecordOrd
import Data.HList.HList hiding (append',
                                hAppend',
                                FHCons(..),
                                hMapAux,
                                MapCar(..),
                                hMapMapCar,
                                hSequence2,
                                )
import Data.HList.HCurry
import Data.HList.HSort
import Data.HList.MakeLabels
import Data.HList.TypeEqO hiding (IsKeyFN)
import Data.HList.TIP
import Data.HList.TIC

import Data.HList.HZip
import Data.HList.Label3 hiding (MapLabel)
import Data.HList.Label5 () -- only instances
import Data.HList.Label6 () -- only instances
import Data.HList.Labelable (Labelable(..),
                             Projected(..), projected',
                             toLabel,
                             (.==.),
                             LabeledOptic)

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

{- $labelable #labelabledemo#

Rather than having the @x = Label :: Label \"x\"@, the labels
generated by 'makeLabelable' also double as lenses for "Control.Lens".
Here is an example of how much better that is:

>>> :set -XNoMonomorphismRestriction -XDataKinds -XPolyKinds
>>> import Control.Lens
>>> import Data.HList.Labelable
>>> let x = hLens' (Label :: Label "x")
>>> let y = hLens' (Label :: Label "y")

The Label6 method:

>>> let r = (Label :: Label "x") .=. "5" .*. emptyRecord

The Labelable way:

>>> let r2 = x .==. "5" .*. emptyRecord

>>> r ^. x
"5"

>>> r2 ^. x
"5"

>>> r & x .~ ()
Record{x=()}

When a field is missing, the error names that field:

>>> :t r^.y
...
...No instance for (Fail (FieldNotFound "y"))
...

-}


