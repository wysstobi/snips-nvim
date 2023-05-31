{-# LANGUAGE OverloadedStrings #-}

module Plugin.SnipsApi (snipsCreate, snipsSave, handleTelescopeSelection, snips) where

import qualified Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Neovim
  ( Alternative (empty),
    CommandArguments (CommandArguments, range),
    asks,
    liftIO,
  )
import Neovim.API.String
  ( Buffer,
    buffer_get_lines,
    buffer_insert,
    buffer_line_count,
    nvim_buf_line_count,
    nvim_exec_lua,
    vim_get_current_buffer,
  )
import Plugin.FileIO.FileIO (loadSnippet, getAllSnippetsByFileType, writeSnippet)
import Plugin.NeovimUtil.Buffer (clearBuffer, closeBuffer, createNewBuf, getBufferFileType, getCurrentCursorPosition, readAndPaste, setCurrentBuffersFileType)
import Plugin.NeovimUtil.Input (askForString)
import Plugin.NeovimUtil.Util (writeToStatusLine)
import Plugin.Text.Text (extractPlaceholders, replaceInText)
import Plugin.Types (Placeholder (Placeholder, key), PlaceholderST, PlaceholderState (..), Snippet (..), SnippetMetaData (..), SnipsEnv (..), SnipsNvim)

-- :lua print(vim.o.filetype) gets the currentfiletypefile

-- | Opens a new buffer containing the currently selected text.
snipsCreate :: CommandArguments -> SnipsNvim ()
snipsCreate CommandArguments {range = _range} =
  case _range of
    (Just (l1, l2)) -> do
      cb <- vim_get_current_buffer
      fileType <- getBufferFileType cb
      newBuffer <- createNewBuf "Create new snippet" Nothing
      setCurrentBuffersFileType fileType
      readAndPaste cb newBuffer l1 l2 0
      return ()
    Nothing -> return ()

-- | Saves the current buffer as a new snippet.
snipsSave :: CommandArguments -> SnipsNvim ()
snipsSave _ = do
  cb <- vim_get_current_buffer
  path <- asks snippetPath
  lineCount <- buffer_line_count cb
  fileType <- getBufferFileType cb
  bufferContent <- buffer_get_lines cb 0 lineCount True
  snippetName <- askForString "Enter a name for the snippet:" Nothing
  let newSnippet = createSnippet snippetName bufferContent fileType
  writeResult <- liftIO $ writeSnippet path newSnippet
  case writeResult of
    Left errorMsg -> writeToStatusLine errorMsg
    Right _ -> do 
      writeToStatusLine $ "Created snippet " ++ snippetName ++ " successfully"
      closeBuffer cb
    where createSnippet snippetName content fileType = Snippet snippetName content (SnippetMetaData [fileType])

-- | Handles the selection of a snippetname by telescope
handleTelescopeSelection :: CommandArguments -> String -> SnipsNvim ()
handleTelescopeSelection _ snippetName = do
  (buffer, line) <- getCurrentCursorPosition
  quotes <- asks qs
  filePath <- asks snippetPath
  snippet <- liftIO $ loadSnippet filePath snippetName
  case snippet of
    Left errorMsg -> writeToStatusLine errorMsg
    Right snippet' -> do
      let state = PS snippet' quotes []
      snipBuffer <- fst <$> runStateT (replacePlaceholders snippet') state
      setCurrentBuffersFileType (intercalate "," (fileTypes (meta snippet')))
      linecount <- fromIntegral <$> nvim_buf_line_count snipBuffer
      readAndPaste snipBuffer buffer 1 linecount line
      closeBuffer snipBuffer

-- | Creates a new buffer and inserts the selected snippet to it
replacePlaceholders :: Snippet -> PlaceholderST Buffer
replacePlaceholders snippet = do
  placeholders <- extractPlaceholders
  buffer <- lift $ createNewBuf ("Insert " <> name snippet) Nothing
  lift $ buffer_insert buffer 0 (content snippet)
  askForPlaceholderReplacements placeholders
  text <- replaceInText

  let replacedText = fromMaybe [] text
  lift $ clearBuffer buffer
  lift $ buffer_insert buffer 0 replacedText
  pure buffer

-- | Ask the user for replacements for the placeholders and store it in the state
askForPlaceholderReplacements :: [Placeholder] -> PlaceholderST ()
askForPlaceholderReplacements [] = pure ()
askForPlaceholderReplacements (current : rest) = do
  PS (Snippet name content meta) qs rs <- get
  let prompt = "Enter a text which replaces \"" ++ key current ++ "\":"
  replacement <- lift $ askForString prompt Nothing
  put $ PS (Snippet name content meta) qs (rs ++ [Placeholder (key current) (Just replacement)])
  askForPlaceholderReplacements rest

-- |  Opens a @Telescope@ finder to select a snippet to insert
snips :: CommandArguments -> SnipsNvim ()
snips _ = do
  -- create a table from all existing snippets
  cb <- vim_get_current_buffer
  ft <- getBufferFileType cb
  filePath <- asks snippetPath
  eitherSnippets <- liftIO $ getAllSnippetsByFileType filePath ft
  case eitherSnippets of 
    Left errorMsg -> writeToStatusLine errorMsg
    Right snippets -> do
      let table = (intercalate "," . map ((\str -> "'" ++ str ++ "'") . name)) snippets
      let command = "return run({" ++ table ++ "})"
      Control.Monad.void (nvim_exec_lua command empty)
