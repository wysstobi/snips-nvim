module Plugin.NeovimUtil.Buffer (createNewBuf, readAndPaste, clearBuffer) where

import Plugin.Environment.SnipsEnvironment (SnipsNvim, names, SnipsEnv (snippetPath))
import Neovim.API.String

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

readAndPaste :: Buffer -> Buffer -> Int -> Int -> SnipsNvim ()
readAndPaste readBuf writeBuf from to = do
  lines <- buffer_get_lines readBuf (fromIntegral from -1) (fromIntegral to) True
  buffer_insert writeBuf 0 lines

clearBuffer :: Buffer -> SnipsNvim ()
clearBuffer buffer = do 
  lineCount <- buffer_line_count buffer
  clearBuffer' lineCount where
    clearBuffer' 0 = pure ()
    clearBuffer' lineCount = buffer_del_line buffer 0 
