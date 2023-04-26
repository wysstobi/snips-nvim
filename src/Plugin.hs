{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Plugin
    ( plugin
    ) where

import Neovim

import Plugin.Random (nextRandom, setNextRandom, randomNumbers)
import Plugin.Fibonacci2 (fibonacci2)
import Plugin.FprodTeam(fprodTeam)

plugin :: Neovim () NeovimPlugin
plugin = do
    randomPluginState <- randomNumbers
    wrapPlugin Plugin
        { environment = randomPluginState
        , exports =
				[ $(function' 'fibonacci2) Sync
			     ,$(function' 'fprodTeam) Sync
            -- Notice the quotation mark before the functin name, this is
            -- important!

            , $(function' 'nextRandom) Sync
            , $(function "SetNextRandom" 'setNextRandom) Async
            ]
        }
