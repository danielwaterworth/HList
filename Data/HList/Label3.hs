{-# LANGUAGE CPP #-}

{- |

   Description : namespaced labels

   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A model of labels as needed for extensible records. As before,
   all the information about labels is recorded in their type, so
   the labels of records may be purely phantom. In general,
   Labels are exclusively type-level entities and have no run-time
   representation.

   Record labels are triplets of type-level naturals, namespace,
   and description. The namespace part helps avoid confusions between
   labels from different Haskell modules. The description is
   an arbitrary nullary type constructor.

   For the sake of printing, the description is required to be the
   instance of Show. One must make sure that the show functions does
   not examine the value, as descr is purely phantom. Here's an
   example of the good Label description:

   >     data MyLabelDescr; instance Show MyLabelDescr where show _ = "descr"

   which are automated by makeLabels3 from "Data.HList.MakeLabel".

   This model even allows the labels in a record to belong to different
   namespaces. To this end, the model employs the predicate for type
   equality.
-}

module Data.HList.Label3 where

import Data.HList.FakePrelude
import Data.Typeable


data Lbl (x :: HNat) (ns :: *) (desc :: *)  -- labels are exclusively type-level entities
#if !OLD_TYPEABLE
  deriving Typeable
#else
instance (ShowLabel x) => Typeable2 (Lbl x) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "HList" "Data.HList.Label3" "Lbl")
    [mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" (showLabel (Label :: Label x)))
      []]
#endif

-- Public constructors for labels

-- | Construct the first label
firstLabel :: ns -> desc -> Label (Lbl HZero ns desc)
firstLabel = undefined


-- | Construct the next label
nextLabel :: Label (Lbl x ns desc) -> desc' -> Label (Lbl (HSucc x) ns desc')
nextLabel = undefined


-- | Equality on labels (descriptions are ignored)
-- Use generic instance
{-
instance ( HEq x x' b
         , HEq ns ns' b'
	 , bres ~ HAnd b b'
         )
      =>   HEq (Lbl x ns desc) (Lbl x' ns' desc') bres
-}

-- | Show label

instance Show desc => ShowLabel (Lbl x ns desc) where
  showLabel = show . getd
      where getd :: Label (Lbl x ns desc) -> desc -- for the sake of Hugs
            getd = undefined

instance Show desc => Show (Label (Lbl x ns desc))
 where
  show = show . getd
      where getd :: Label (Lbl x ns desc) -> desc -- for the sake of Hugs
            getd = undefined

