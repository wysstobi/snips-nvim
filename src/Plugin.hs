{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Plugin
    ( plugin,
    ) where

import Neovim

import Plugin.FprodTeam(fprodTeam)
import Plugin.SnipsApi(snipsCreate, snipsSave, handleTelescopeSelection, snips)
import Plugin.Types (SnipsEnv(..))


plugin :: Neovim () NeovimPlugin
plugin = do
    wrapPlugin Plugin
        { environment = SnipsEnv { names = "Tobi, Andri, Raphi", snippetPath = "mysnippets.json", qs = ("<#", "#>") },
          exports = [ 
            $(function' 'fprodTeam) Sync, 
            $(command'  'snipsCreate) [CmdRange WholeFile],
            $(command'  'snipsSave) [CmdRange WholeFile],
            $(command'  'handleTelescopeSelection) [CmdSync Async], 
            $(command'  'snips) []
          ]
        }
