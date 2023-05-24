module Plugin.Text.ParsersTest where

import Test.Tasty
import Test.Tasty.HUnit
import Plugin.Text.Text (getReplacementForKey)

parsersTests :: TestTree
parsersTests = testGroup "parsers" [testEmptyReplacement]

testEmptyReplacement :: TestTree
testEmptyReplacement = testCase "test getReplacementForKey2" $
  getReplacementForKey [] "" @?= ""
