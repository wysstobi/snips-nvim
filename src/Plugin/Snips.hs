{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Snips
    ( snipsCreate, snipsSave
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath))
import Neovim.API.String
import Control.Monad (when)
import Data.String (IsString(fromString))

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
  -- TODO does not work properly
  snippetName <- show <$> nvim_execute_lua "vim.fn.input(\"Enter a name for the snippet: \")" []
  liftIO $ writeFile (path ++ snippetName ++ ".json") (foldr (\cur acc -> cur ++ "\n" ++ acc) "" bufferContent)
  vim_command "bd!"


readAndPaste :: Buffer -> Buffer -> Int -> Int -> SnipsNvim () 
readAndPaste readBuf writeBuf from to = do
  lines <- buffer_get_lines readBuf (fromIntegral from -1) (fromIntegral to) True
  buffer_insert writeBuf 0 lines
  return ()

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
