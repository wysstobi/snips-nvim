module Plugin.NeovimUtil.Buffer (createNewBuf, readAndPaste, clearBuffer,
getCurrentCursorPosition,closeBuffer) where

import Neovim.API.String
import Neovim.Classes (NvimObject(fromObjectUnsafe))
import Plugin.Types (SnipsNvim)

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

readAndPaste :: Buffer -> Buffer -> Int -> Int -> Int -> SnipsNvim ()
readAndPaste readBuf writeBuf from to insertAt = do
  lines <- buffer_get_lines readBuf (fromIntegral from -1) (fromIntegral to) True
  buffer_insert writeBuf (fromIntegral insertAt) lines

clearBuffer :: Buffer -> SnipsNvim ()
clearBuffer buffer = do 
  lineCount <- buffer_line_count buffer
  clearBuffer' lineCount where
    clearBuffer' 0 = pure ()
    clearBuffer' lineCount = do
      buffer_del_line buffer (lineCount -1)
      clearBuffer' (lineCount -1)


getCurrentCursorPosition :: SnipsNvim (Buffer, Int)
getCurrentCursorPosition = do 
  currentBuffer <- nvim_get_current_buf
  currentLineNumber <- fromObjectUnsafe <$> vim_eval "line(\".\")"
  return (currentBuffer, currentLineNumber)


closeBuffer :: Buffer -> SnipsNvim ()
closeBuffer buffer = do
  vim_set_current_buffer buffer
  vim_command "bd!"
