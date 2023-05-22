{-# LANGUAGE OverloadedStrings #-}

module Plugin.SnipsApi
    ( snipsCreate, snipsSave, handleTelescopeSelection, snips) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath, quotes))
import Neovim.API.String
import Control.Monad (when, guard, liftM2, liftM3)
import Data.String (IsString(fromString))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Plugin.NeovimUtil.Buffer (createNewBuf, readAndPaste, clearBuffer)
import Plugin.NeovimUtil.Input (askForString)
import Plugin.FileIO.FileIO (loadSnippet, allSnippets)
import Plugin.Types  (Snippet(..), PlaceholderState(..), Placeholder (Placeholder, key, value), PlaceholderST, Quotes, PlaceholderSTOld)
import Plugin.Text.Text (extractPlaceholders, replaceInText)
import Control.Monad.Trans.State (StateT(runStateT), get, put)
import Control.Monad.Trans.Class


-- :lua print(vim.o.filetype) gets the currentfiletypefile

-- | Opens a new buffer containing the currently selected text.
snipsCreate :: CommandArguments -> SnipsNvim ()
snipsCreate CommandArguments { range = _range }  =
  case _range of
    (Just (l1, l2)) -> do
      cb <- vim_get_current_buffer
      newBuffer <- createNewBuf "Create new snippet" Nothing
      readAndPaste cb newBuffer l1 l2
      return ()
    Nothing -> return ()

-- | Saves the current buffer as a new snippet.
snipsSave :: CommandArguments -> SnipsNvim ()
snipsSave _ = do
  cb <- vim_get_current_buffer
  path <- asks snippetPath
  lineCount <- buffer_line_count cb
  bufferContent <- buffer_get_lines cb 0 lineCount True
  snippetName <- askForString "Enter a name for the snippet:" Nothing
  liftIO $ writeFile (path ++ snippetName ++ ".json") (foldr (\cur acc -> cur ++ "\n" ++ acc) "" bufferContent)
  vim_command "bd!"

-- | Handles the selection of a snippetname by telescope
handleTelescopeSelection :: CommandArguments -> String -> SnipsNvim ()
handleTelescopeSelection _ snippetName = do 
  quotes <- asks Plugin.Environment.SnipsEnvironment.quotes
  snippet <- liftIO $ loadSnippet snippetName
  let state = PS snippet quotes []
  fst <$> runStateT (replacePlaceholders snippet) state

-- | Creates a new buffer and inserts the selected snippet to it
replacePlaceholders :: Snippet -> PlaceholderST ()
replacePlaceholders snippet = do
  placeholders <- extractPlaceholders 
  buffer <- lift $ createNewBuf ("Insert " <> name snippet) Nothing
  lift $ buffer_insert buffer 0 (content snippet)
  replacements <- askForPlaceholderReplacements placeholders
  text <- replaceInText 

  let replacedText = fromMaybe [] text
  lift $ clearBuffer buffer 
  lift $ buffer_insert buffer 0 replacedText
  pure ()

-- | Ask the user for replacements for the placeholders and store it in the state
askForPlaceholderReplacements :: [Placeholder] -> PlaceholderST ()
askForPlaceholderReplacements [] = pure ()
askForPlaceholderReplacements (current:rest) = do
  PS (Snippet name content) qs rs <- get
  let prompt = "Enter a text which replaces \""  ++ key current ++ "\":"
  replacement <- lift $ askForString prompt Nothing
  put $ PS (Snippet name content) qs (rs ++ [Placeholder (key current) (Just replacement)])
  askForPlaceholderReplacements rest

-- | Opens a @Telescope@ finder to select a snippet to insert
snips :: CommandArguments -> SnipsNvim ()
snips _ = do
  -- create a table from all existing snippets
  let table = (intercalate "," . map ((\str -> "'" ++ str ++ "'") . name)) allSnippets

  let command = "return run({" ++ table ++"})"
  nvim_exec_lua command empty
  pure ()
