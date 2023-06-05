{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}

module Plugin.Text.SearchingTest where
import Data.List
import Test.Tasty
import Data.Set
import Test.Tasty.QuickCheck
import Plugin.Types (Placeholder (..), PlaceholderState (..), Snippet (..), SnippetMetaData (SnippetMetaData))
import Plugin.Text.Parsers ( parse)
import Plugin.Text.Searching
import Control.Monad.State (evalState)
import Plugin.Text.ReplacingTest (addQuoteToString, quote)

searchingTests :: TestTree
searchingTests = testGroup "Text Searching" [
  testPlaceholderSetFromStrings,
  testParseSingle,
  testSetFromList,
  testPlaceholdersInEmptyLine,
  testSinglePlaceholderInLine,
  testMultiplePlaceholderInLine,
  testEmptySnippetExtractPlaceholders,
  testSnippetExtractPlaceholders
  ]

generateSnippetText :: String -> String -> Placeholder -> Placeholder -> Placeholder -> [String]
generateSnippetText s1 s2 ph1 ph2 ph3 = [
      s1 ++ addQuoteToString(key ph1) ++ s2,
      addQuoteToString(key ph2) ++ s1 ++ s2,
      s1 ++ s2 ++ addQuoteToString(key ph3), 
      s1 ++ s2 ++ addQuoteToString(key ph1)
  ]

-- extractPlaceholders
testEmptySnippetExtractPlaceholders :: TestTree
testEmptySnippetExtractPlaceholders = testProperty "test extract placeholders from empty snippet" $
  \ph ->
     evalState
       extractPlaceholders
       (PS (Snippet "name" [] (SnippetMetaData ["hs"])) quote [ph :: Placeholder]) == []

testSnippetExtractPlaceholders :: TestTree
testSnippetExtractPlaceholders = testProperty "test multiple extract placeholders from snippet" $
  \s1 s2 ph1 ph2 ph3->
  Data.List.foldr (&&) True (fmap ((/= "") . key) [ph1, ph2, ph3]) ==>
  Data.List.foldr (&&) True (fmap (\el -> not $ fst quote `isInfixOf` key el) [ph1, ph2, ph3]) ==>
  Data.List.foldr (&&) True (fmap (\el -> not $ snd quote `isInfixOf` key el) [ph1, ph2, ph3]) ==>
  Data.List.foldr (&&) True (fmap (\el -> not $ snd quote `isInfixOf` el) [(s1 :: String), (s2 :: String)]) ==>
  Data.List.foldr (&&) True (fmap (\el -> not $ fst quote `isInfixOf` el) [s1, s2]) ==>
    sort (evalState extractPlaceholders (
      PS (
       Snippet "name" (generateSnippetText s1 s2 ph1 ph2 ph3) (SnippetMetaData ["hs"])
          ) quote [ph1, ph2, ph3])) 
      ==  sort (toList $ fromList (fmap (\ph -> Placeholder (key ph) Nothing) [ph1, ph2, ph3]))

-- placeholdersInLine
testPlaceholdersInEmptyLine :: TestTree
testPlaceholdersInEmptyLine = testProperty "test find placeholder in empty line" $
     \ph ->
     evalState
       (placeholdersInLine "")
       (PS (Snippet "name" [] (SnippetMetaData ["hs"])) quote [ph :: Placeholder]) == []

testSinglePlaceholderInLine :: TestTree
testSinglePlaceholderInLine = testProperty "test find single placeholder in line" $
     \ph ->
     evalState
       (placeholdersInLine ("Some text" ++ addQuoteToString (key ph) ++ "lksjdfk"))
       (PS (Snippet "name" [] (SnippetMetaData ["hs"])) quote [ph :: Placeholder]) == [key ph]

testMultiplePlaceholderInLine :: TestTree
testMultiplePlaceholderInLine = testProperty "test find multiple placeholders in line" $
     \(pre, middle, post, ph1, ph2) ->
     key ph1 /= "" ==>
     key ph2 /= "" ==>
     not (snd quote `isInfixOf` key ph1) ==>
     not (snd quote `isInfixOf` key ph2) ==>
     not (fst quote `isInfixOf` pre) ==>
     not (fst quote `isInfixOf` post) ==>
     not (fst quote `isInfixOf` middle) ==>
     evalState
       (placeholdersInLine (pre ++ addQuoteToString (key ph1) ++ middle ++ addQuoteToString (key ph2) ++ post))
       (PS (Snippet "name " [] (SnippetMetaData ["hs"])) quote [ph1, ph2]) == [key ph1, key ph2]

-- parseSingle
testParseSingle :: TestTree
testParseSingle = testProperty "test parseSingle with arbitrary pre and suffix" $
   \pre post valueOfInterest ->
     not (fst quote `isInfixOf` (pre :: String)) ==>
     not (snd quote `isInfixOf` (pre :: String)) ==>
     not (snd quote `isInfixOf` (valueOfInterest :: String)) ==>
     valueOfInterest /= "" ==>
      parse (parseSingle quote) (pre ++ fst quote ++ valueOfInterest ++ snd quote ++ (post :: String)) == Just (valueOfInterest, post)

-- placeholderSetFromStrings
testPlaceholderSetFromStrings :: TestTree
testPlaceholderSetFromStrings = testProperty "test placeholdersFromStrings" $
  \x -> placeholderSetFromStrings [x :: String] == [Placeholder x Nothing]

-- setFromList
testSetFromList :: TestTree
testSetFromList = testProperty "test create a set from a list" $
  \xs -> Plugin.Text.Searching.setFromList (xs :: [Int]) == toList (fromList xs)
