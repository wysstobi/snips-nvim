{-# LANGUAGE OverloadedStrings #-}

module Plugin.FileIO.FileIO (allSnippets, loadSnippet, snippetsOfType, updateSnippet, addNewSnippet) where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    eitherDecode,
    encode,
    object,
    withObject,
    (.:),
  )
import qualified Data.ByteString.Lazy as B
import Plugin.Types (Snippet (..), SnippetMetaData (..))
import System.Directory (doesFileExist)

filePath :: FilePath
filePath = "mysnippets.json"

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
    snippets <- v .: "snippets"
    return $ SnippetList snippets

instance ToJSON SnippetList where
  toJSON (SnippetList snippets) = object ["snippets" .= snippets]

writeSnippet :: Snippet -> IO ()
writeSnippet snippet' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet existingSnippets snippet'
  -- Write updated snippets to file
  B.writeFile filePath (encode updatedSnippets)

-- Read existing snippets from file or create an empty list
readExistingSnippets :: IO SnippetList
readExistingSnippets = do
  exists <- doesFileExist filePath
  if exists
    then do
      -- Read existing snippets from file
      bytes <- B.readFile filePath
      case eitherDecode bytes of
        Left err -> error $ "Failed to decode JSON: " ++ err
        Right snippetList -> return snippetList
    else return (SnippetList [])

allSnippets :: IO SnippetList
allSnippets = do
  -- Read existing snippets from file or create an empty list
  readExistingSnippets

loadSnippet :: String -> IO (Maybe Snippet)
loadSnippet name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' == name s) (snippets existingSnippets)
  -- Return first snippet if it exists
  return (if null filteredSnippets then Nothing else Just (head filteredSnippets))

addNewSnippet :: SnippetList -> Snippet -> SnippetList
addNewSnippet (SnippetList snippets) newSnippet =
  if any (\s -> name s == name newSnippet) snippets
    then SnippetList snippets
    else SnippetList (newSnippet : snippets)

updateSnippet :: Snippet -> IO ()
updateSnippet snippet' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name snippet' /= name s) (snippets existingSnippets)
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet (SnippetList filteredSnippets) snippet'
  -- Write updated snippets to file
  B.writeFile filePath (encode updatedSnippets)

getAllSnippetsByFileType :: String -> IO [Snippet]
getAllSnippetsByFileType type' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by file type
  let filteredSnippets = filter (\s -> type' `elem` fileTypes (meta s)) (snippets existingSnippets)
  return filteredSnippets

snippetsOfType :: String -> IO [Snippet]
snippetsOfType type' = do
  getAllSnippetsByFileType type'

deleteSnippetByName :: String -> IO ()
deleteSnippetByName name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' /= name s) (snippets existingSnippets)
  -- Write updated snippets to file
  B.writeFile filePath (encode (SnippetList filteredSnippets))

checkIfSnippetExists :: String -> IO Bool
checkIfSnippetExists name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' == name s) (snippets existingSnippets)
  return (not (null filteredSnippets))