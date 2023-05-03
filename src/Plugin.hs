{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Plugin
    ( plugin
    ) where

import Neovim

import Plugin.Random (nextRandom, setNextRandom, randomNumbers)
import Plugin.Fibonacci(fibonacci)
import Plugin.FprodTeam(fprodTeam)
import Plugin.Snips(snipsCreate, snipsSave)
import Plugin.Environment.SnipsEnvironment
import GHC.Conc
import Neovim.Plugin.Classes (mkCommandOptions)


plugin :: Neovim () NeovimPlugin
plugin = do
    randomPluginState <- randomNumbers
    wrapPlugin Plugin
        { environment = SnipsEnv { randomState = randomPluginState, names = "Tobi, Andri, Raphi", snippetPath = "~/snippets/"},
          exports = [ 
            $(function' 'fibonacci) Sync, 
            $(function' 'fprodTeam) Sync, 
            $(command' 'snipsCreate) [CmdRange WholeFile],
            $(command' 'snipsSave) [CmdRange WholeFile],
            $(function' 'nextRandom) Sync,
            $(function "SetNextRandom" 'setNextRandom) Async
          ]
        }
