{-# LANGUAGE ConstraintKinds,
        DataKinds,
        EmptyDataDecls,
        ExistentialQuantification,
        FlexibleContexts,
        FlexibleInstances,
        GADTs,
        KindSignatures,
        MultiParamTypeClasses,
        PolyKinds,
        RankNTypes,
        ScopedTypeVariables,
        TypeFamilies,
        TypeOperators,
        UndecidableInstances #-}
{- | 'Data.Data.Data' instances for 'HList' and 'Record' which pretend
to be flat data structures.


[@HList@]

The Data instance for

> a :: Data b => HList '[Int,Double,b]

will look like the Data instance for:

> data A b = A Int Double b

[@Record@]

For 'Record' similar ideas apply. An

> a :: Record '[ LVPair "x" Int, LVPair "y" Double ]

should behave like a:

> data A = A { x :: Int, y :: Double } deriving (Data)

Many unsafecoerces are necessary here because the Data class includes type
parameters @c@ that cannot be used in the class context for the instance.
Perhaps there is another way.

-}
module Data.HList.Data (
    -- * exports for type signatures/ haddock usage
    DataHListCxt,
    DataRecordCxt,
    TypeRepsList,

    -- ** less likely to be used
    RecordLabelsStr(..),
    GfoldlK(..),
    GunfoldK(..),
    ) where

import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.Label6
import GHC.TypeLits
import Data.Data
import Data.Typeable
import Data.List

import Unsafe.Coerce

type DataHListCxt g a = (HBuild' '[] g,
        Typeable (HList a),
        HFoldl (GfoldlK  C) (C g) a (C (HList a)),

        HFoldr
            (GunfoldK C)
            (C g)
            (HReplicateR (HLength a) ())
            (C (HList a)),

        HReplicate (HLength a) ())

instance DataHListCxt g a => Data (HList a) where

    gfoldl k z xs = c3 $
                    hFoldl
                        (c1 (GfoldlK k))
                        (c2 (z hBuild))
                        xs
        where
              c1 :: forall c. GfoldlK c -> GfoldlK C
              c1 = unsafeCoerce

              c2 :: forall c. c g -> C g
              c2 = unsafeCoerce

              c3 :: forall c. C (HList a) -> c (HList a)
              c3 = unsafeCoerce

    gunfold k z _ =
              c3 $ withSelf $ \self ->
                hFoldr
                    (c1 (GunfoldK k))
                    (c2 (z hBuild))
                    (hReplicate (hLength self) ())
        where
              withSelf :: forall t c. (t -> c t) -> c t
              withSelf x = x undefined

              c1 :: forall c. GunfoldK c -> GunfoldK C
              c1 = unsafeCoerce

              c2 :: forall c. c g -> C g
              c2 = unsafeCoerce

              c3 :: forall c. C (HList a) -> c (HList a)
              c3 = unsafeCoerce

    dataTypeOf _ = hListDataRep
    toConstr _   = hListConRep

hListDataRep = mkDataType "Data.HList.HList" [hListConRep]
hListConRep = mkConstr hListDataRep "HList" [] Prefix

type DataRecordCxt a =
    (Data (HList (RecordValuesR a)),
            TypeRepsList a,
            RecordValues a,
            RecordLabelsStr a)

instance DataRecordCxt a => Data (Record a) where
    gfoldl k z xs = c1 (gfoldl k z (recordValues xs))
        where
            c1 :: forall c. c (HList (RecordValuesR a)) -> c (Record a)
            c1 = unsafeCoerce

    gunfold k z con = c1 (gunfold k z con)
        where
            -- LVPair and Record are newtypes, so this should be safe...
            c1 :: forall c. c (HList (RecordValuesR a)) -> c (Record a)
            c1 = unsafeCoerce

    dataTypeOf x = snd (recordReps (recordLabelsStr x))
    toConstr x = fst (recordReps (recordLabelsStr x))


recordReps fields =
    let c = mkConstr d "Record" fields Prefix
        d = mkDataType "Data.HList.Record" [c]
    in (c,d)



class RecordLabelsStr (xs :: [*]) where
    recordLabelsStr :: Record xs -> [String]

instance RecordLabelsStr '[] where
    recordLabelsStr _ = []
instance (RecordLabelsStr xs,
          ShowLabel x) => RecordLabelsStr (LVPair x t ': xs) where
    recordLabelsStr _ = showLabel (undefined :: Label x) :
                            recordLabelsStr (undefined :: Record xs)

{- |

This alternative option works too, but for whatever reason
splitting up recordLabelsStr and recordLabels into two functions
means that a type annotation is needed on the 3, which is not
necessary with the above recordLabelsStr (ghc-7.6.3)

> recordLabelsStr2 (recordLabels (((Label :: Label "x") .=. 3 .*. emptyRecord )))

-}
class RecordLabelsStr2 (xs :: [k]) where
    recordLabelsStr2 :: Proxy xs -> [String]

instance RecordLabelsStr2 '[] where
    recordLabelsStr2 _ = []
instance (RecordLabelsStr2 xs,
          ShowLabel x) => RecordLabelsStr2 (x ': xs) where
    recordLabelsStr2 _ = showLabel (undefined :: Label x) :
                            recordLabelsStr2 (undefined :: Proxy xs)


-- | use only with @instance Data (HList a)@. This is because the HFoldl
-- context cannot be written for a @c@ that only appears in the method
-- 'gfoldl'.
data C a


-- | should be derived for ghc-7.8
instance TypeRepsList xs => Typeable (HList xs) where
   typeOf x = mkTyConApp (mkTyCon3 "HList" "Data.HList.HList" "HList")
                [ tyConList (typeRepsList (Record x)) ]

-- | should be derived for ghc-7.8
instance (TypeRepsList xs) => Typeable (Record xs) where
  typeOf x = mkTyConApp (mkTyCon3 "HList" "Data.HList.Record" "Record")
                [ tyConList (typeRepsList x) ]

-- pretty-prints sort of like a real list
tyConList xs = mkTyConApp open ( intersperse comma xs ++ [close] )
    where
    open = mkTyCon3 "GHC" "GHC.TypeLits" "["
    close = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" "]") []
    comma = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" ",") []


class TypeRepsList a where
  typeRepsList :: Record a -> [TypeRep]

instance (TypeRepsList xs, Typeable x) => TypeRepsList (x ': xs) where
  typeRepsList (Record (x `HCons` xs))
        = typeOf (undefined :: x) : typeRepsList (Record xs)

instance TypeRepsList '[] where
  typeRepsList _ = []

instance ShowLabel sy => Typeable1 (LVPair sy) where
  typeOf1 _ = mkTyConApp
        (mkTyCon3 "HList" "Data.HList.Data" (showLabel (undefined :: Label sy)))
        []

instance (ShowLabel sy, Typeable x) => Typeable (LVPair sy x) where
  typeOf _ = mkTyConApp
            (mkTyCon3 "GHC" "GHC.TypeLits" (showLabel (undefined :: Label sy)))
            [mkTyConApp (mkTyCon3 "HList" "Data.HList.Record" "=") [],
                    typeOf (undefined :: x)
                    ]


-- | wraps up the first argument to 'gfoldl'
data GfoldlK c = GfoldlK (forall d b. Data d => c (d -> b) -> d -> c b)

instance (Data d, (c (d -> b), d) ~ x, c b ~ y) =>
        ApplyAB (GfoldlK c) x y where
    applyAB (GfoldlK f) (u,v) = f u v


data GunfoldK c = GunfoldK (forall b r. Data b => c (b -> r) -> c r)

instance (Data b, x ~ (t, c (b -> r)), y ~ c r) =>
        ApplyAB (GunfoldK c) x y where
    applyAB (GunfoldK f) (_, u) = f u


