{- | Util functions to interact with NeoVim buffers. -}
module Plugin.NeovimUtil.Buffer (createNewBuf, readAndPaste, clearBuffer, getBufferFileType,
getCurrentCursorPosition,closeBuffer, setCurrentBuffersFileType) where

import Neovim.API.String
import Neovim.Classes (NvimObject(fromObjectUnsafe))
import Plugin.Types (SnipsNvim)

-- | Creates a new buffer.
createNewBuf :: String  -- ^ the name of the new buffer
             -> Maybe Buffer -- ^ the buffer to add the focus to. If not set, the newly created buffer will be focused.
             -> String -- ^ the filetype of the new buffer
             -> SnipsNvim Buffer -- ^ the newly created buffer
createNewBuf bufferName focus bufferFileType = case focus of
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
        setCurrentBuffersFileType bufferFileType
        vim_get_current_buffer

-- | Reads the content of a given buffer and pastes it to another buffer.
readAndPaste :: Buffer -- ^ the buffer to read the content from
             -> Buffer -- ^ the buffer to insert the content to
             -> Int -> Int  -- ^ from this line to the other the content will be read
             -> Int    -- ^ the content will be pasted starting on  this line
             -> SnipsNvim ()
readAndPaste readBuf writeBuf from to insertAt = do
  linesInBuffer <- buffer_get_lines readBuf (fromIntegral from -1) (fromIntegral to) True
  buffer_insert writeBuf (fromIntegral insertAt) linesInBuffer

-- | Clears the content of the given buffer.
clearBuffer :: Buffer -> SnipsNvim ()
clearBuffer buffer = do 
  lineCount <- buffer_line_count buffer
  clearBuffer' lineCount where
    clearBuffer' 0 = pure ()
    clearBuffer' lineCount = do
      buffer_del_line buffer (lineCount -1)
      clearBuffer' (lineCount -1)

-- | Reads the filetype of the given buffer.
getBufferFileType :: Buffer -> SnipsNvim String
getBufferFileType buffer = fromObjectUnsafe <$> buffer_get_option buffer "filetype"

-- | Updates the filetpye of the current buffer.
setCurrentBuffersFileType :: String -> SnipsNvim()
setCurrentBuffersFileType fileType = vim_command $ "set filetype=" ++ fileType

-- | Gets the linenumber and the buffer where the cursor is located.
getCurrentCursorPosition :: SnipsNvim (Buffer, Int)
getCurrentCursorPosition = do 
  currentBuffer <- nvim_get_current_buf
  currentLineNumber <- fromObjectUnsafe <$> vim_eval "line(\".\")"
  return (currentBuffer, currentLineNumber)

-- | Closes the given buffer.
closeBuffer :: Buffer -> SnipsNvim ()
closeBuffer buffer = do
  vim_set_current_buffer buffer
  vim_command "bd!"
