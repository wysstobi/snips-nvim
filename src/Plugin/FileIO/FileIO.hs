{-# LANGUAGE OverloadedStrings #-}

module Plugin.FileIO.FileIO (readExistingSnippets2, readExistingSnippets, loadSnippet, getAllSnippetsByFileType, writeSnippet) where

import Data.Aeson
  ( eitherDecode,
    encode,
  )
import qualified Data.ByteString.Lazy as B
import Plugin.Types (Snippet (..), SnippetMetaData (..), SnippetList (SnippetList, snippets))
import System.Directory (doesFileExist)

writeSnippet :: FilePath -> Snippet -> IO (Either String ())
writeSnippet path snippet' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets path
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet snippet'<$> existingSnippets 
  -- Write updated snippets to file
  case updatedSnippets of 
    Right s -> do 
      B.writeFile path (Data.Aeson.encode s)
      return $ Right ()
    Left errMsg -> return $ Left errMsg

-- Read existing snippets from file
readExistingSnippets :: FilePath -> IO (Either String SnippetList)
readExistingSnippets snippetsFilePath = do
  exists <- doesFileExist snippetsFilePath
  if exists then do
    -- Read existing snippets from file
    bytes <- B.readFile snippetsFilePath
    return $ case eitherDecode bytes of
      Left err -> Left $ "Failed to decode JSON: " ++ err
      Right snippetList -> Right snippetList
  else 
    return $ Left "The snippet file does not exist"

-- | loads a single snippet by its name
loadSnippet :: FilePath -> String -> IO (Either String Snippet)
loadSnippet snippetsPath name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets snippetsPath

  -- Filter snippets by name
  return (existingSnippets >>= safeHead . filterByName name')
    where filterByName snippetName = filter (\s -> snippetName == name s) . snippets
          safeHead l = if null l then Left "No snippet with this name found" else Right $ head l

addNewSnippet :: Snippet -> SnippetList -> SnippetList
addNewSnippet newSnippet (SnippetList snippets') =
  if any (\s -> name s == name newSnippet) snippets'
    then SnippetList snippets'
    else SnippetList (newSnippet : snippets')

-- updateSnippet :: Snippet -> IO ()
-- updateSnippet snippet' = do
--   -- Read existing snippets from file or create an empty list
--   existingSnippets <- readExistingSnippets
--   -- drop snippet to update
--   let filteredSnippets = filter (\s -> name snippet' /= name s) . snippets  <$> existingSnippets
--   -- Add new snippet to the list
--   let updatedSnippets = addNewSnippet (SnippetList filteredSnippets) snippet'
--   -- Write updated snippets to file
--   B.writeFile filePath (Data.Aeson.encode updatedSnippets)

getAllSnippetsByFileType :: FilePath -> String -> IO (Either String [Snippet])
getAllSnippetsByFileType snippetsPath type' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets snippetsPath
  -- Filter snippets by file type
  let filteredSnippets = filter (\s -> type' `elem` fileTypes (meta s)) . snippets <$> existingSnippets
  return filteredSnippets


-- deleteSnippetByName :: String -> IO ()
-- deleteSnippetByName name' = do
--   -- Read existing snippets from file or create an empty list
--   existingSnippets <- readExistingSnippets
--   -- Filter snippets by name
--   let filteredSnippets = filter (\s -> name' /= name s) (snippets existingSnippets)
--   -- Write updated snippets to file
--   B.writeFile filePath (Data.Aeson.encode (SnippetList filteredSnippets))
-- 
-- checkIfSnippetExists :: String -> IO Bool
-- checkIfSnippetExists name' = do
--   -- Read existing snippets from file or create an empty list
--   existingSnippets <- readExistingSnippets
--   -- Filter snippets by name
--   let filteredSnippets = filter (\s -> name' == name s) (snippets existingSnippets)
--   return (not (null filteredSnippets))
