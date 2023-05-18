{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Plugin.Types () where

data Snippet = Snippet { name :: String, content :: [String] }



