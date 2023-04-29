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
import Plugin.Environment.SnipsEnvironment
import GHC.Conc


plugin :: Neovim () NeovimPlugin
plugin = do
    randomPluginState <- randomNumbers
    wrapPlugin Plugin
        { environment = SnipsEnv { randomState = randomPluginState, names = "Tobi, Andri, Raphi" },
          exports = [ 
            $(function' 'fibonacci) Sync, 
            $(function' 'fprodTeam) Sync, 
            $(function' 'nextRandom) Sync,
            $(function "SetNextRandom" 'setNextRandom) Async
          ]
        }
