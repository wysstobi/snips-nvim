{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.FprodTeam
    ( fprodTeam, currentSelection
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names)
import Neovim.API.String
import Control.Monad (when)

fprodTeam :: SnipsNvim String
fprodTeam = asks names

-- currentSelection :: SnipsNvim String
-- currentSelection = return "hello"

currentSelection :: CommandArguments -> SnipsNvim ()
currentSelection CommandArguments { range=_range }  =
  case _range of
    (Just (l1, l2)) -> do
      cb <- vim_get_current_buffer

      lines <- buffer_get_lines cb (fromIntegral l1) (fromIntegral l2) True
      buffer_insert cb 0 $ map ("--" ++) lines

      return ()
    Nothing -> return ()





