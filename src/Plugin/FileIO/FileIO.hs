{-# LANGUAGE OverloadedStrings #-}

module Plugin.FileIO.FileIO (readFileAndTransform, loadSnippet, getAllSnippetsByFileType, writeSnippet) where

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
  existingSnippets <- readFileAndTransform path snippetTransformer
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet snippet'<$> existingSnippets
  -- Write updated snippets to file
  case updatedSnippets of
    Right s -> do
      B.writeFile path (Data.Aeson.encode s)
      return $ Right ()
    Left errMsg -> return $ Left errMsg

-- | Read existing snippets from file
readFileAndTransform :: FilePath -> (B.ByteString -> Either String a)-> IO (Either String a)
readFileAndTransform snippetsFilePath transformer = do
  exists <- doesFileExist snippetsFilePath
  if exists then do
    -- Read existing snippets from file
    bytes <- B.readFile snippetsFilePath
    return $ transformer bytes
  else
    return $ Left "The requested file does not exist"

-- | loads a single snippet by its name
loadSnippet :: FilePath -> String -> IO (Either String Snippet)
loadSnippet snippetsPath name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readFileAndTransform snippetsPath snippetTransformer

  -- Filter snippets by name
  return (existingSnippets >>= safeHead . filterByName name')
    where filterByName snippetName = filter (\s -> snippetName == name s) . snippets
          safeHead l = if null l then Left "No snippet with this name found" else Right $ head l

addNewSnippet :: Snippet -> SnippetList -> SnippetList
addNewSnippet newSnippet (SnippetList snippets') =
  if any (\s -> name s == name newSnippet) snippets'
    then SnippetList snippets'
    else SnippetList (newSnippet : snippets')


getAllSnippetsByFileType :: FilePath -> String -> IO (Either String [Snippet])
getAllSnippetsByFileType snippetsPath type' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readFileAndTransform snippetsPath snippetTransformer
  -- Filter snippets by file type
  let filteredSnippets = filter (\s -> type' `elem` fileTypes (meta s)) . snippets <$> existingSnippets
  return filteredSnippets

snippetTransformer :: B.ByteString -> Either String SnippetList
snippetTransformer bytes = case eitherDecode bytes of
      Left err -> Left $ "Failed to decode JSON: " ++ err
      Right snippetList -> Right snippetList

-- Following functions are not used yet, but might be of interest in further development of the plugin.
deleteSnippetByName :: FilePath -> String -> IO (Either String ())
deleteSnippetByName snippetsPath name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readFileAndTransform snippetsPath snippetTransformer
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' /= name s) . snippets <$> existingSnippets
  -- Write updated snippets to file
  case filteredSnippets of
    Right s -> do
      B.writeFile snippetsPath (Data.Aeson.encode (SnippetList s))
      return $ Right ()
    Left errMsg -> return $ Left errMsg

checkIfSnippetExists :: FilePath -> String -> IO Bool
checkIfSnippetExists snippetsPath name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readFileAndTransform snippetsPath snippetTransformer 
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' == name s) . snippets <$> existingSnippets
  return (not (null filteredSnippets))

updateSnippet :: FilePath -> Snippet -> IO (Either String ())
updateSnippet snippetsPath snippet' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readFileAndTransform snippetsPath snippetTransformer 
  -- drop snippet to update
  let filteredSnippets = filter (\s -> name snippet' /= name s) . snippets  <$> existingSnippets
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet snippet' . SnippetList <$> filteredSnippets
  -- Write updated snippets to file
  case updatedSnippets of
    Right s -> do
      B.writeFile snippetsPath (Data.Aeson.encode s)
      return $ Right ()
    Left errMsg -> return $ Left errMsg
