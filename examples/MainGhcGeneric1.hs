{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a main module for exercising a model with generic type
   cast and generic type equality. Because of generic type equality,
   this model works with GHC but it does not work with Hugs.

-}

module MainGhcGeneric1 where

import Datatypes2
import Data.HList.CommonMain

reproxy :: proxy a -> Proxy a
reproxy _ = Proxy


-- --------------------------------------------------------------------------

type Animal =  '[Key,Name,Breed,Price]

angus :: HList Animal
angus =  HCons (Key 42)
           (HCons (Name "Angus")
           (HCons  Cow
           (HCons (Price 75.5)
            HNil)))

tList1 = hFoldr (HSeq HPrint) (return () :: IO ()) angus
{-
 Key 42
 Name "Angus"
 Cow
 Price 75.5
-}

tList2 = print $ hAppend angus angus
{-
H[Key 42, Name "Angus", Cow, Price 75.5, Key 42, Name "Angus", Cow, Price 75.5]
-}


testListBasic = do
  putStrLn "\nBasic HList tests"
  tList1
  tList2

testHArray = do
  putStrLn "\ntestHArray"
  myProj1
  myProj2
  myProj2'
  myProj3
  myProj4

myProj1 = print $ hProjectByHNats (hNats (HCons hZero (HCons hZero HNil))) angus
-- H[Key 42]

-- Before:
-- H[Key 42, Key 42]
-- XXX I don't duplicate at present!

myProj2 = print $ 
	  hProjectByHNats (hNats (HCons hZero (HCons (hSucc hZero) HNil))) angus
-- H[Key 42, Name "Angus"]

myProj2' = print $ 
	  hProjectByHNats (undefined::Proxy ['HZero, 'HSucc 'HZero]) angus
-- H[Key 42, Name "Angus"]

myProj3 = print $ 
	  hProjectAwayByHNats (hNats (HCons hZero HNil)) angus
-- H[Name "Angus", Cow, Price 75.5]

myProj4 = print $ 
	  hSplitByHNats 
	    (undefined::Proxy ['HZero, 'HSucc 'HZero])
	    angus
-- (H[Key 42, Name "Angus"],H[Cow, Price 75.5])

testHOccurs = do
  putStrLn "\ntestHOccurs"
  print (hOccurs angus :: Breed)
  print $ hOccurs (TIP (HCons 1 HNil))
  print $ null $ hOccurs (TIP (HCons [] HNil))
  print (hProject angus :: HList '[Key, Name])


testTypeIndexed = do
  putStrLn "\ntestTypeIndexed"
  print typeIdx1
  print typeIdx2
  print $ hUpdateAt Sheep typeIdx1
  print $ hDeleteAt (undefined::Proxy Breed) typeIdx2
  print $ hProjectBy (undefined::Proxy '[Breed]) angus
  print $ hSplitBy (undefined:: Proxy '[Breed]) angus
 where 
  typeIdx1 = hDeleteMany (undefined::Proxy Name) angus
  typeIdx2 = BSE .*. angus

-- |
-- This example from the TIR paper challenges singleton lists.
-- Thanks to the HW 2004 reviewer who pointed out the value of this example.
-- We note that the explicit type below is richer than the inferred type.
-- This richer type is needed for making this operation more polymorphic.
-- That is, /a)/ would not work without the explicit type, 
-- while /b/ would:
--
-- >  a)  ((+) (1::Int)) $ snd $ tuple oneTrue
-- >  b)  ((+) (1::Int)) $ fst $ tuple oneTrue

tuple l = tipyTuple l


-- | A specific tuple
-- Need to import an instance of TypeEq to be able to run the examples
oneTrue :: TIP '[Int, Bool]		-- inferred
oneTrue = (1::Int) .*. True .*. emptyTIP

testTuple = do
  putStrLn "\ntestTuple"
  print $ let (a,b) = tuple oneTrue in (a+(1::Int), not b)
  print $ let b = not $ fst $ tuple oneTrue in (1::Int,b)
  print $ tuple oneTrue == (1::Int,True)
  print $ ((+) (1::Int)) $ fst $ tuple oneTrue
  -- requires explicit type for tuple
  print $ ((+) (1::Int)) $ snd $ tuple oneTrue


myTipyCow = TIP angus

animalKey :: ( SubType l (TIP Animal) -- explicit
             , HOccurs Key l          -- implicit
             ) => l -> Key
animalKey = hOccurs

animalish :: SubType l (TIP Animal) => l -> l
animalish = id
animalKey' l = hOccurs (animalish l) :: Key

testTIP = do
  putStrLn "\ntestTIP"
  print $ (hOccurs myTipyCow :: Breed)
  print $ BSE .*. myTipyCow
  -- print $ Sheep .*. myTipyCow
  {- if we uncomment the line above, we get the type error
     about the violation of the TIP condition: Breed type
     occurs twice.

    No instance for (Fail * (TypeFound Breed))
      arising from a use of `hExtend'
  -}
  print $ Sheep .*. tipyDelete (undefined::Proxy Breed) myTipyCow
  print $ tipyUpdate Sheep myTipyCow

{-
data MyNS = MyNS -- a name space for record labels

key   = firstLabel MyNS  (undefined::DKey)
name  = nextLabel  key   (undefined::DName)
breed = nextLabel  name  (undefined::DBreed)
price = nextLabel  breed (undefined::DPrice)

data DKey;   instance Show DKey   where show _ = "key"
data DName;  instance Show DName  where show _ = "name"
data DBreed; instance Show DBreed where show _ = "breed"
data DPrice; instance Show DPrice where show _ = "price"

-}

makeLabels3 "MyNS" (words "key name breed price")

unpricedAngus =  key    .=. (42::Integer)
             .*. name   .=. "Angus"
             .*. breed  .=. Cow
             .*. emptyRecord


getKey l = hLookupByLabel key l

testRecords = do
  putStrLn "\ntestRecords"
  print $ unpricedAngus
  print $ unpricedAngus .!. breed
  print $ test3
  print $ test4
  print $ test5
  print $ hProjectByLabels (labelsOf (breed `HCons` price `HCons` HNil)) test5
 where
  test3 = hDeleteAtLabel breed unpricedAngus
  test4 = breed .=. Sheep .@. unpricedAngus
  test5 = price .=. 8.8 .*. unpricedAngus
  -- test7 should have the same type as unpricedAngus and test4 but
  -- with the different order of labels
  test7 = (newLVPair breed Sheep) .*. test3

{-
testRecords =   ( test1 
              , ( test2
              , ( test3 
              , ( test4
              , ( test5
              , ( test6
	      , (test7, test81, test82, test83, test84, test85)
                ))))))
 where
  test81 = equivR test1 test3 -- HNothing
  test82 = let HJust (r17,r71) = equivR test1 test7 in (r17 test1,r71 test7)
  test83 = let HJust (r17,r71) = 
		   equivR test1 test7 in show (r17 test1) == show test7
  test84 = let HJust (r47,r74) = 
		   equivR test4 test7 in (show (r47 test4) == show test7,
					  show (r74 test7) == show test4)
  test85 = let HJust (r7,r7') = 
		   equivR test7 test7 in show (r7 test7) == show (r7' test7)

testRecordsP =   ( test1 
		 , ( test2
		 , ( test3 
		 , ( test4
		 , ( test5
		 , ( test6
                   ))))))
 where
--  test1 = mkRecordP (undefined::Animal) angus
  test1 = record_r2p unpricedAngus
  test2 = test1 .!. breed
  test3 = hDeleteAtLabelP breed test1
--  test4 = test1 .@. breed .=. Sheep
  test4 = hExtend (newLVPair breed Sheep) test3
  test5 = price .=. 8.8 .*. test1
  test6 = fst $ h2projectByLabels (HCons breed (HCons price HNil)) test5


-}

type AnimalCol = [Key,Name,Breed,Price]

testTIC = do
  putStrLn "\ntestTIC"
  print $ myCol
  print $ (unTIC myCol :: Maybe Breed)
  print $ (unTIC myCol :: Maybe Price)
 where
  myCol = mkTIC Cow :: TIC AnimalCol

{-

myCol = mkTIC Cow :: TIC AnimalCol

*TIC> unTIC myCol :: Maybe Breed
Just Cow
*TIC> unTIC myCol :: Maybe Price
Nothing
*TIC> mkTIC "42" :: TIC AnimalCol
Type error ...
*TIC> unTIC myCol :: Maybe String
Type error ...

-}

testVariant = do
    putStrLn "\ntestVariant"
    print testVar1
    print testVar2
    print testVar3
 where
  testVar1 = mkVariant name "angus" animalVar
  testVar2 = testVar1 .!. key
  testVar3 = testVar1 .!. name

animalVar = reproxy $ 
               key   .=. (undefined :: Integer)
           .*. name  .=. (undefined :: String)
           .*. breed .=. (undefined :: Breed)
           .*. emptyRecord
{-
-- --------------------------------------------------------------------------

main = mainExport
mainExport
   = print $   ( testHArray
               , ( testHOccurs
               , ( testTypeIndexed
               , ( testTuple
               , ( testTIP

               , ( testRecords
               , ( testRecordsP
               , ( testTIC
               , ( testVariant
               )))))))))

-}

main = do
       testListBasic
       testHArray
       testHOccurs
       testTypeIndexed
       testTuple
       testTIP
       testRecords
       testTIC
       testVariant
