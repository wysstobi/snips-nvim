{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Plugin.Snips (Snippet) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath))
import Neovim.API.String
import Control.Monad (when, guard)
import Data.String (IsString(fromString))
import Data.List (intercalate)






insertSnippet :: String -> SnipsNvim ()
insertSnippet snippetName = do


data Snippet = Snippet { name :: String, content :: [String] }



