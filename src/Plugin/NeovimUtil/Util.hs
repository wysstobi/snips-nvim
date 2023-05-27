{-# LANGUAGE OverloadedStrings #-}

module Plugin.NeovimUtil.Util where

import Neovim.API.String (vim_command)
import Plugin.Types (SnipsNvim)

writeToStatusLine :: String -> SnipsNvim ()
writeToStatusLine str = vim_command ("lua print(\"" ++ str ++ "\")")
