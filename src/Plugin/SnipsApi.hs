{-# LANGUAGE OverloadedStrings #-}

module Plugin.SnipsApi
    ( snipsCreate, snipsSave, handleTelescopeSelection, snips
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath))
import Neovim.API.String
import Control.Monad (when, guard)
import Data.String (IsString(fromString))
import Data.List (intercalate)
import Plugin.NeovimUtil.Buffer (createNewBuf, readAndPaste)
import Plugin.NeovimUtil.Input (askForString)
import Plugin.FileIO.FileIO (loadSnippet, allSnippets, Snippet(..))


-- :lua print(vim.o.filetype) gets the currentfiletype

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


handleTelescopeSelection :: CommandArguments -> String -> SnipsNvim ()
handleTelescopeSelection _ snippetName = do
  snippet <- liftIO $ loadSnippet snippetName
  buffer <- createNewBuf ("Insert " <> name snippet) Nothing
  buffer_insert buffer 0 (content snippet)
  pure ()


-- | Opens a @Telescope@ finder to select a snippet to insert
snips :: CommandArguments -> SnipsNvim ()
snips _ = do
  -- create a table from all existing snippets
  let table = (intercalate "," . map ((\str -> "'" ++ str ++ "'") . name)) allSnippets

  let command = "return run({" ++ table ++"})"
  nvim_exec_lua command empty
  pure ()
