{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified BinomialHeap as BH

main :: IO ()
main = $(defaultMainGenerator)

case_1 :: Assertion
case_1 = do 1 @=? BH.findMin h4
           where h1 = BH.insert 4 []
                 h2 = BH.insert 3 h1
                 h3 = BH.insert 1 h2
                 h4 = BH.insert 2 h3
