{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Plugin.Types (Snippet(..)) where

data Snippet = Snippet { name :: String, content :: [String] }



