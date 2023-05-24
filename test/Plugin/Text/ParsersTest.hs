module Plugin.Text.ParsersTest where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Plugin.Text.Parsers
import Test.Tasty.QuickCheck

parsersTests :: TestTree
parsersTests = testGroup "parsers" [
  testSimpleParseUntil, 
  testQuoteSignBeforeQuoteParseUntil, 
  testPrefixParseUntil,
  testSuffixParseUntil,
  testPreAndSuffixParseUntil
  ]

quote :: String
quote = "<>"

testSimpleParseUntil :: TestTree
testSimpleParseUntil = testCase "test parseUntil simple case" $
  parse (parseUntil quote) "123<>abc" @?= Just ("123","abc")

testQuoteSignBeforeQuoteParseUntil :: TestTree
testQuoteSignBeforeQuoteParseUntil = testCase "test parseUntil with char from quote before quote" $
  parse (parseUntil quote) ("123" ++ take 1 quote ++ quote ++ "abc") @?= Just ("123" ++ take 1 quote,"abc")

testPrefixParseUntil :: TestTree
testPrefixParseUntil  = testProperty "test parseUntil with arbitrary prefix" $
   \x ->
     not (quote `isInfixOf` (x :: String)) ==>
      parse (parseUntil quote) (x ++ quote) == Just (x, "")

testSuffixParseUntil :: TestTree
testSuffixParseUntil = testProperty "test parseUntil with arbitrary suffix" $
   \x ->
     not (quote `isInfixOf` (x :: String)) ==>
      parse (parseUntil quote) (quote ++ x) == Just ("", x)

testPreAndSuffixParseUntil :: TestTree
testPreAndSuffixParseUntil = testProperty "test parseUntil with arbitrary pre and suffix" $
   \x y ->
     not (quote `isInfixOf` (x :: String)) ==>
     not (quote `isInfixOf` (y :: String)) ==>
      parse (parseUntil quote) (x ++ quote ++ y) == Just (x, y)
