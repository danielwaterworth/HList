module GhcSyntax where

import HListPrelude

infixr 1 :*:
infixr 1 .*.
 
type e :*: l = HCons e l
 
(.*.) :: HExtend e l l' => e -> l -> l'
(.*.) =  hExtend
