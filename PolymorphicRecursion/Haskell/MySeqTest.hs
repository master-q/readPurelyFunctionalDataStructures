{-# LANGUAGE TemplateHaskell #-}
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import MySeq

fooTestGroup :: Test.Framework.Test
fooTestGroup = $(testGroupGenerator)

main :: IO ()
main = defaultMain [fooTestGroup]

case_0 :: Assertion
case_0 = 0 @=? sizeS MSNil

case_1 :: Assertion
case_1 = 1 @=? sizeS (MSCons (1, MSNil))

case_3 :: Assertion
case_3 = 3 @=? sizeS (MSCons (1, MSCons ((2, 3), MSNil)))
