module Datatypes1 where

-- The fout-n-mouth example

newtype Key     = Key Integer
                deriving (Show,Eq,Ord)
newtype Name   = Name String
                deriving (Show,Eq)
data Breed     = Cow | Sheep
                deriving (Show,Eq)
newtype Price  = Price Float
                deriving (Show,Eq,Ord)
data Disease   = BSE | FM 
                deriving (Show,Eq)
