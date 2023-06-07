{-# LANGUAGE OverloadedStrings #-}

module Plugin.Types where
import Control.Monad.Trans.State (StateT)
import Neovim
import Data.Aeson.Types

-- Environment
data SnipsEnv = SnipsEnv { snippetPath :: String, qs :: Quotes }

type SnipsNvim a = Neovim SnipsEnv a

type Quotes = (String, String)

newtype SnippetMetaData = SnippetMetaData { fileTypes :: [String] } deriving (Show, Eq)

data Snippet = Snippet { name :: String, content :: [String], meta :: SnippetMetaData } deriving (Show, Eq)


-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving (Show, Eq, Ord)

-- instance Eq Placeholder where
--   (Placeholder k v) == (Placeholder k2 v2) = k == k2 && v == v2

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving (Show, Eq)


type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a


-- | 'Aeson' instance for snippet metadata
-- Used to create a JSON object from snippet metadata
instance FromJSON SnippetMetaData where
  parseJSON = withObject "SnippetMetaData" $ \v -> do
    fileTypes <- v .: "fileTypes"
    return $ SnippetMetaData fileTypes

{- | 'Aeson' instance for SnippetMetaData
Used to create a JSON object from SnippetMetaData

__Example:__
@
meta : {
  "fileTypes": [
    "haskell",
    "python"
  ]
}
@
-}
instance ToJSON SnippetMetaData where
  toJSON (SnippetMetaData fileTypes) = object ["fileTypes" .= fileTypes]

-- | 'Aeson' instance for Snippet
-- Used to create a snippet from a JSON object
instance FromJSON Snippet where
  parseJSON = withObject "Snippet" $ \v -> do
    name <- v .: "name"
    content <- v .: "content"
    meta <- v .: "meta"
    return $ Snippet name content meta


{- | 'Aeson' instance for Snippet
Used to create a JSON object from a snippet

__Example:__

@
{
  "name": "Best Snippet Ever",
  "content": [
    "main :: IO ()",
    "main = putStrLn \"Hello, World!\"\n"
  ],
  "meta": {...}
}
@
-}
instance ToJSON Snippet where
  toJSON (Snippet name content meta) = object ["name" .= name, "content" .= content, "meta" .= meta]

-- | Type used to represent a list of snippets
newtype SnippetList = SnippetList {snippets :: [Snippet]} deriving (Show)

-- | 'Aeson' instance for SnippetList
-- Used to create a list of snippets from a JSON object
instance FromJSON SnippetList where
  -- | Parse a JSON object into a SnippetList
  parseJSON = withObject "SnippetList" $ \v -> do
    -- | Get the list of snippets from the JSON object
    snippets' <- v .: "snippets"
    -- | Return a SnippetList
    return $ SnippetList snippets'

-- | 'Aeson' instance for SnippetList
-- Used to create a JSON object from a list of snippets
instance ToJSON SnippetList where
  -- | Create a JSON object from a SnippetList
  toJSON (SnippetList snippets') = object ["snippets" .= snippets']
