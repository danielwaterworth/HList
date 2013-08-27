-- Test of Data.HList.HSequence

module HSequenceTest where

import Data.HList
import Control.Monad (liftM2)


hlist = HCons (Just (1 :: Integer)) (HCons (Just 'c') HNil) -- Maybe monad
hlist2 = HCons ([1]) (HCons (['c']) HNil)      -- List monad

testHSequence  = hSequence hlist
testHSequence2 = hSequence hlist2

main :: IO ()
main = do
       print testHSequence
       print testHSequence2
