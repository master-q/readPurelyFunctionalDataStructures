module Main (main) where
import Test.HUnit
import qualified Queue as Q
import qualified BatchedQueue as BQ

e1, e2, e3 :: Int
(e1, e2, e3) = (1, 2, 3)
q0, q1, q2, q3, q4, q5, q6 :: BQ.BatchedQueue Int
q0 = Q.empty
q1 = q0 `Q.snoc` e1
q2 = q1 `Q.snoc` e2
q3 = Q.tail q2
q4 = q3 `Q.snoc` e3
q5 = Q.tail q4
q6 = Q.tail q5

testEmpty :: Test
testEmpty = let r = (all Q.isEmpty [q0, q6])
            in  TestCase $ assertBool "isEmpty of empty failed" r

testNotEmpty :: Test
testNotEmpty = let r = (not . any Q.isEmpty) [q1, q2, q3, q4, q5]
               in  TestCase $ assertBool "isEmpty of non-empty failed" r

-- testBadHead :: Test
-- currently unable to test error case
-- see http://hackage.haskell.org/trac/ghc/ticket/5129

testHead :: Test
testHead = let r = (and . zipWith (==) [e1, e1, e2, e2, e3] . map Q.head)
                   [q1, q2, q3, q4, q5]
           in  TestCase $ assertBool "head failed" r

-- testBadTail :: Test
-- currently unable to test error case
-- see http://hackage.haskell.org/trac/ghc/ticket/5129

testTail :: Test
testTail = let r = (and . zipWith (==) [q0, q3, q5, q0] . map Q.tail)
                   [q1, q2, q4, q5]
           in  TestCase $ assertBool "tail failed" r

tests :: Test
tests = TestList [TestLabel "testEmpty" testEmpty,
                  TestLabel "testNotEmpty" testNotEmpty,
                  TestLabel "testHead" testHead,
                  TestLabel "testTail" testTail]

main :: IO Counts
main = runTestTT tests
