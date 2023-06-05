{-# LANGUAGE OverloadedStrings #-}

module SnipsTest where
import Test.Tasty
import Plugin.Text.SearchingTest (searchingTests)
import Plugin.Text.ReplacingTest (replacingTests)

main :: IO ()
main =  defaultMain $ testGroup "snips tests" [
   searchingTests, replacingTests 
   ]

