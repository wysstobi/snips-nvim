{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Snips
    ( snipsCreate, snipsSave, test, tele
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath))
import Neovim.API.String
import Control.Monad (when, guard)
import Data.String (IsString(fromString))
import Data.List (intercalate)

snipsCreate :: CommandArguments -> SnipsNvim ()
snipsCreate CommandArguments { range = _range }  =
  case _range of
    (Just (l1, l2)) -> do
      cb <- vim_get_current_buffer
      newBuffer <- createNewBuf "Create new snippet" Nothing

      readAndPaste cb newBuffer l1 l2
      return ()
    Nothing -> return ()

snipsSave :: CommandArguments -> SnipsNvim ()
snipsSave _ = do
  cb <- vim_get_current_buffer
  path <- asks snippetPath
  lineCount <- buffer_line_count cb
  bufferContent <- buffer_get_lines cb 0 lineCount True
  snippetName <- askForString "Enter a name for the snippet:" Nothing
  liftIO $ writeFile (path ++ snippetName ++ ".json") (foldr (\cur acc -> cur ++ "\n" ++ acc) "" bufferContent)
  vim_command "bd!"


readAndPaste :: Buffer -> Buffer -> Int -> Int -> SnipsNvim ()
readAndPaste readBuf writeBuf from to = do
  lines <- buffer_get_lines readBuf (fromIntegral from -1) (fromIntegral to) True
  buffer_insert writeBuf 0 lines
  return ()



insertSnippet :: String -> SnipsNvim ()
insertSnippet snippetName = do
  snippet <- liftIO $ loadSnippet snippetName
  buffer <- createNewBuf ("Insert " <> name snippet) Nothing
  buffer_insert buffer 0 (content snippet)


data Snippet = Snippet { name :: String, content :: [String] }

testSnippet :: [Snippet]
testSnippet = [Snippet "MySnippet" ["hello <#Title#>"], Snippet "Andris Snippet" ["bye <#Title#>"]]

loadSnippet :: String -> IO Snippet
loadSnippet n = do
    let s = filter (\(Snippet name _) -> name == n ) testSnippet
    pure $ head s

-- | Helper function that calls the @input()@ function of neovim.
input :: NvimObject result
      => String -- ^ Message to display
      -> Maybe String -- ^ Input fiiled in
      -> Maybe String -- ^ Completion mode
      -> SnipsNvim result

input message mPrefilled mCompletion = fmap fromObjectUnsafe
  $ vim_call_function "input" $ (message <> " ")
    +: maybe "" id mPrefilled
    +: maybe [] (+: []) mCompletion

askForString :: String -- ^ message to put in front
             -> Maybe String -- ^ Prefilled text
             -> SnipsNvim String
askForString message mPrefilled = input message mPrefilled Nothing

createNewBuf :: String -> Maybe Buffer -> SnipsNvim Buffer
createNewBuf bufferName focus = case focus of
  Nothing -> createNewBuf'
  Just aBuffer -> do
    cb <- createNewBuf'
    vim_set_current_buffer aBuffer
    return cb
  where
    createNewBuf' = do
        vim_command "new"
        newBuffer <- vim_get_current_buffer
        buffer_set_name newBuffer bufferName
        vim_get_current_buffer

test :: CommandArguments -> String -> SnipsNvim ()
test _ s = do
  --value <- fmap fromObjectUnsafe $ nvim_exec_lua ("return MyFunction('" <> s <> "')") empty  
  insertSnippet s
  pure ()


tele :: CommandArguments -> SnipsNvim ()
tele _ = do
  -- create a table from all existing snippets
  let table = (intercalate "," . map ((\str -> "'" ++ str ++ "'") . name)) testSnippet

  -- let table = foldr (\(Snippet name _) acc -> acc ++ ", '" ++ name ++ "'") "{" testSnippet ++ "}"
  let command = "return run({" ++ table ++"})"
  nvim_exec_lua command empty
  pure ()
