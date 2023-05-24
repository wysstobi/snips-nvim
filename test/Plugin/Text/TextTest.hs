module Plugin.Text.TextTest where

import Test.Tasty
import Test.Tasty.HUnit
import Plugin.Text.Text (getReplacementForKey)

textTests :: TestTree 
textTests = testGroup "text" [testEmptyReplacement]

testEmptyReplacement :: TestTree
testEmptyReplacement = testCase "test getReplacementForKey" $
  getReplacementForKey [] "" @?= ""
