{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.FprodTeam
    ( fprodTeam
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names)
import Neovim.API.String
import Control.Monad (when)

fprodTeam :: SnipsNvim String
fprodTeam = asks names

