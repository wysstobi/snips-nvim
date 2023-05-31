{-# LANGUAGE OverloadedStrings #-}

module Plugin.Types where
import Control.Monad.Trans.State (StateT)
import Neovim
import Data.Aeson.Types

-- Environment
data SnipsEnv = SnipsEnv { names :: String, snippetPath :: String, qs :: Quotes }

newtype SnippetMetaData = SnippetMetaData { fileTypes :: [String] } deriving Show

type SnipsNvim a = Neovim SnipsEnv a
data Snippet = Snippet { name :: String, content :: [String], meta :: SnippetMetaData } deriving Show

type Quotes = (String, String)
-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving Show

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving Show


type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a

-- JSON
instance FromJSON SnippetMetaData where
  parseJSON = withObject "SnippetMetaData" $ \v -> do
    fileTypes <- v .: "fileTypes"
    return $ SnippetMetaData fileTypes

instance ToJSON SnippetMetaData where
  toJSON (SnippetMetaData fileTypes) = object ["fileTypes" .= fileTypes]

instance FromJSON Snippet where
  parseJSON = withObject "Snippet" $ \v -> do
    name <- v .: "name"
    content <- v .: "content"
    meta <- v .: "meta"
    return $ Snippet name content meta

instance ToJSON Snippet where
  toJSON (Snippet name content meta) = object ["name" .= name, "content" .= content, "meta" .= meta]

newtype SnippetList = SnippetList {snippets :: [Snippet]} deriving (Show)

instance FromJSON SnippetList where
  parseJSON = withObject "SnippetList" $ \v -> do
    snippets' <- v .: "snippets"
    return $ SnippetList snippets'

instance ToJSON SnippetList where
  toJSON (SnippetList snippets') = object ["snippets" .= snippets']
