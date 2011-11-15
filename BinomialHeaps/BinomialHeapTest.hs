{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified BinomialHeap as BH

main :: IO ()
main = $(defaultMainGenerator)

case_1 :: Assertion
case_1 = (1::Int) @=? BH.findMin (BH.insertAll [2,3,1,4,5,9,8,7,6] [])

case_2 :: Assertion
case_2 = (1::Int) @=? BH.findMin' (BH.insertAll [2,3,1,4,5,9,8,7,6] [])
