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

-- | Helper function that calls the @input()@ function of neovim.
input :: NvimObject result
      => String -- ^ Message to display
      -> Maybe String -- ^ Input fiiled in
      -> Maybe String -- ^ Completion mode
      -> Neovim env result
input message mPrefilled mCompletion = fmap fromObjectUnsafe
  $ vim_call_function "input" $ (message <> " ")
    +: maybe "" id mPrefilled
    +: maybe [] (+: []) mCompletion

askForString :: String -- ^ message to put in front
             -> Maybe String -- ^ Prefilled text
             -> Neovim env String
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
  value <- fmap fromObjectUnsafe $ nvim_exec_lua ("return MyFunction('" <> s <> "')") empty  
  askForString value Nothing
  pure () 
  
tele :: CommandArguments -> SnipsNvim () 
tele _ = do
  nvim_exec_lua ("return run({'Das Erste', 'Champions League', 'Bier saufen'})") empty  
  pure () 
--test :: CommandArguments -> SnipsNvim env 
-- test = fromObjectUnsafe <$> vim_call_function "expand" [ObjectBinary "%:p:h"]

