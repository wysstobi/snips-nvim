{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.FprodTeam
    ( fprodTeam
    ) where

import Neovim
import GHC.Conc
import Neovim.API.String
import Control.Monad (when)
import Plugin.Types(SnipsNvim, SnipsEnv (..))

fprodTeam :: SnipsNvim String
fprodTeam = asks names

