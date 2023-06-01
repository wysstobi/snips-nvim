{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plugin.Text.TextTest where

import Data.List
import Data.Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Plugin.Text.Text (getReplacementForKey, placeholderSetFromStrings, parseSingle, setFromList, extractPlaceholders, replaceInLine, replaceNext)
import Plugin.Types (Placeholder (..), Quotes, PlaceholderST, PlaceholderState (..), Snippet (..), SnippetMetaData (SnippetMetaData))
import Plugin.Text.Parsers ( parse)
import Control.Monad.RWS (MonadState , MonadTrans (lift))
import Control.Monad.Trans.State (StateT(runStateT), get, put, runState)
import Control.Monad.State (evalState)
textTests :: TestTree
textTests = testGroup "text" [
  testGetReplacementForEmptyKey,
  testEmptyReplacement,
  testPlaceholderSetFromStrings,
  testParseSingle,
  testSetFromList,
 testReplaceNext,
 testReplaceInLine
  ]

instance Arbitrary Snippet where
  arbitrary = do
    name <- listOf $ elements ['a' .. 'z']
    content <- listOf $ listOf $ elements ['a' .. 'z']
    Snippet name content <$> arbitrary

instance Arbitrary SnippetMetaData where
  arbitrary = do
    fileType <- elements ["lua", "hs"]
    pure $ SnippetMetaData [fileType]

instance Arbitrary Placeholder where
  arbitrary = do
    key <- listOf $ elements ['a' .. 'z']
    value <- arbitrary
    pure $ Placeholder key value

testGetReplacementForEmptyKey :: TestTree
testGetReplacementForEmptyKey = testCase "test getReplacementForKey from empty list" $
  getReplacementForKey [] "" @?= ""

placeholder :: Placeholder
placeholder = Placeholder "<>" (Just "fprod")

testEmptyReplacement :: TestTree
testEmptyReplacement = testCase "test getReplacementForKey" $
  getReplacementForKey [placeholder] "<>" @?= "fprod"

testPlaceholderSetFromStrings :: TestTree
testPlaceholderSetFromStrings = testProperty "test placeholdersFromStrings" $
  \x -> placeholderSetFromStrings [x :: String] == [Placeholder x Nothing]

quote :: Quotes
quote = ("<",">")

testParseSingle :: TestTree
testParseSingle = testProperty "test parseSingle with arbitrary pre and suffix" $
   \x y ->
     not (fst quote `isInfixOf` (x :: String)) ==>
     not (snd quote `isInfixOf` (x :: String)) ==>
      parse (parseSingle quote) (x ++ "<aAbBcC123>" ++ (y :: String)) == Just ("aAbBcC123", y)


testSetFromList :: TestTree
testSetFromList = testProperty "test create a set from a list" $
  \xs -> setFromList (xs :: [Int]) == toList (fromList xs)


testSnippet :: Snippet
testSnippet = Snippet "test" ["test1", "test2"] (SnippetMetaData ["hs"])

emptySnippet :: Snippet
emptySnippet = Snippet "" [] (SnippetMetaData [""])

testReplaceInLine :: TestTree
testReplaceInLine = testProperty "test replace in a line" $
  \x ->
     not (fst quote `isInfixOf` (x :: String)) ==>
     not (snd quote `isInfixOf` (x :: String)) ==>
     --((runState (replaceInLine "") (PS testSnippet quote [placeholder])) == (Just "", (PS testSnippet quote [placeholder])))
     evalState (replaceInLine (x ++ "<one>" ++ x) ) (PS emptySnippet quote [Placeholder "one" (Just "fprod")]) == Just "<test>"


testReplaceNext :: TestTree
testReplaceNext = testProperty "test replace next element" $
  \x ->
     not (fst quote `isInfixOf` (x :: String)) ==>
     not (snd quote `isInfixOf` (x :: String)) ==>
       parse (replaceNext [Placeholder "one" (Just "frpod")] ("<",">")) "abc<one>def" == Just ("123","def")
