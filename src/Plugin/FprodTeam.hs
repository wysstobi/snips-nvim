module Plugin.FprodTeam
    ( fprodTeam 
    ) where

import Neovim
import GHC.Conc
import Plugin.Environment.SnipsEnvironment (SnipsNvim, names)

fprodTeam :: SnipsNvim String
fprodTeam = do
   names <$> ask

