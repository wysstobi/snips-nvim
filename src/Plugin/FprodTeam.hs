module Plugin.FprodTeam(fprodTeam) where

import Neovim
import Plugin.Types(SnipsNvim, SnipsEnv (..))

fprodTeam :: SnipsNvim String
fprodTeam = asks names

