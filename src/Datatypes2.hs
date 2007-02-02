{-# OPTIONS -fglasgow-exts #-}

module Datatypes2 where

import Data.Typeable

-- The fout-n-mouth example
-- (deriving Typeable only supported for GHC)

newtype Key     = Key Integer
                deriving (Show,Eq,Ord,Typeable)
newtype Name   = Name String
                deriving (Show,Eq,Typeable)
data Breed     = Cow | Sheep
                deriving (Show,Eq,Typeable)
newtype Price  = Price Float
                deriving (Show,Eq,Ord,Typeable)
data Disease   = BSE | FM 
                deriving (Show,Eq,Typeable)
