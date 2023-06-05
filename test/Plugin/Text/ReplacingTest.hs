{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plugin.Text.ReplacingTest where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Plugin.Types (Placeholder (..), Quotes, PlaceholderState (..), Snippet (..), SnippetMetaData (SnippetMetaData))
import Plugin.Text.Parsers ( parse)
import Control.Monad.State (evalState)
import Plugin.Text.Replacing (replaceInText, replaceNext, getReplacementForKey, replaceInLine)
import Data.Maybe (isJust)
import Test.QuickCheck ( Arbitrary, elements, (==>) )
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Gen (listOf)
import Test.Tasty.QuickCheck (testProperty)

replacingTests :: TestTree
replacingTests = testGroup "Text Replacing" [
  testEmptyListGetReplacementForKey,
  testSingleGetReplacementForKey,
  testArbitraryGetReplacementForKey,
  testMultipleGetReplacementForKey,
  testNonMatchingGetReplacementForKey,
  testReplaceNext,
  testReplaceInLine,
  testReplaceInText
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
    Placeholder key <$> arbitrary

-- TODO: read from config
quote :: Quotes
quote = ("<#","#>")

emptySnippet :: Snippet
emptySnippet = Snippet "" [] (SnippetMetaData [""])

placeholderModul :: Placeholder
placeholderModul = Placeholder "modul" (Just "fprod")

placeholderSchool :: Placeholder
placeholderSchool = Placeholder "school" (Just "fhnw")

addQuoteToString :: String -> String
addQuoteToString s = fst quote ++ s ++ snd quote

createSnippetFromPlaceholderKey :: Placeholder -> Snippet
createSnippetFromPlaceholderKey (Placeholder k _) =
  Snippet "name" (replaceInTestText (addQuoteToString k)) (SnippetMetaData ["hs"])

createSnippetTextFromPlaceholderValue :: Placeholder -> [String]
createSnippetTextFromPlaceholderValue (Placeholder _ Nothing) = replaceInTestText ""
createSnippetTextFromPlaceholderValue (Placeholder _ (Just v)) = replaceInTestText v

replaceInTestText :: String -> [String]
replaceInTestText value = [
    "The first line of a Snippet with key" ++ value,
    value ++ "is a key at the beginning of a line",
    "In the last line, the key" ++ value ++ "is in the middle of the text"
  ]

-- replaceInText
testReplaceInText :: TestTree
testReplaceInText = testProperty "test replace in text" $
  \ph ->
     evalState
       replaceInText
         (PS (createSnippetFromPlaceholderKey ph) quote [ph :: Placeholder])
            == Just (createSnippetTextFromPlaceholderValue ph)

-- replaceNext
testReplaceNext :: TestTree
testReplaceNext = testProperty "test replace next element" $
  \(pre, post, ph) ->
     not (fst quote `isInfixOf` (pre :: String)) ==>
     not (fst quote `isInfixOf` (post :: String)) ==>
     key ph /= "" ==>
     isJust (value ph) ==>
       parse
          (replaceNext [ph::Placeholder] quote)
          (pre ++ fst quote ++ key ph ++ snd quote ++ (post::String))
              == result (value ph) pre post where
                 result (Just a) pre post = Just (pre ++ a, post)
                 result Nothing pre post  = Just (pre, post)

-- replaceInLine
testReplaceInLine :: TestTree
testReplaceInLine = testProperty "test replace placeholder in a line" $
  \(pre, post, ph) ->
     not (fst quote `isInfixOf` (pre :: String)) ==>
     not (fst quote `isInfixOf` (post :: String)) ==>
     key ph /= ""   ==>
     --((runState (replaceInLine "") (PS testSnippet quote [placeholder])) == (Just "", (PS testSnippet quote [placeholder])))
     evalState
       (replaceInLine (pre ++ fst quote ++ key ph ++ snd quote ++ (post::String)))
       (PS emptySnippet quote [ph :: Placeholder]) == result (value ph) pre post where
         result (Just a) pre post = Just (pre ++ a ++ post)
         result Nothing pre post  = Just (pre ++ post)


-- getReplacementForKey
testEmptyListGetReplacementForKey :: TestTree
testEmptyListGetReplacementForKey = testCase "test empty list getReplacementForKey" $
  getReplacementForKey [] "" @?= ""

testSingleGetReplacementForKey :: TestTree
testSingleGetReplacementForKey = testCase "test single placeholder getReplacementForKey" $
  getReplacementForKey [placeholderModul] "modul" @?= "fprod"

testMultipleGetReplacementForKey:: TestTree
testMultipleGetReplacementForKey = testCase "test multiple placeholder getReplacementForKey" $
  getReplacementForKey [placeholderModul, placeholderSchool] "school" @?= "fhnw"

testNonMatchingGetReplacementForKey:: TestTree
testNonMatchingGetReplacementForKey = testCase "test non mathing placeholder getReplacementForKey" $
  getReplacementForKey [placeholderModul, placeholderSchool] "club" @?= ""

testArbitraryGetReplacementForKey :: TestTree
testArbitraryGetReplacementForKey = testProperty "test arbitrary placeholder getReplacementForKey" $
 \ph ->
    getReplacementForKey [ph :: Placeholder] (key ph) == result (value ph) where
         result (Just a) = a
         result Nothing  = ""
