{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SnipsTest where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List
import Plugin.Text.SearchingTest (searchingTests)
import Plugin.Text.ReplacingTest (replacingTests)

main =  defaultMain $ testGroup "snips tests" [
   searchingTests, replacingTests 
   ]

