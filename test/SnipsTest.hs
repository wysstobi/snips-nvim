{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SnipsTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List
import Plugin.Text.Text (getReplacementForKey)

import Plugin.Text.TextTest(textTests)
import Plugin.Text.ParsersTest(parsersTests)

main =  defaultMain $ testGroup "snips tests" [
   textTests, parsersTests
   ]




-- Task 1.1
-- ========
-- 
-- Verify that sorting a list does not change it's length
--
-- Note that we have to use a type annotation to force the type of the list elements. This is necessary
-- as QuickCheck can only generate random values of a concrete type, but not for an unknown type variable.
testPreservesLength :: TestTree
testPreservesLength = testProperty "Sort preserves length" $
  -- placeholder - modify as necessary
  \xs -> (length $ sort (xs :: [Int])) == length xs
