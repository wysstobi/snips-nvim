module Plugin.Environment.SnipsEnvironment where

import GHC.Conc
import Data.Int
import Neovim


-- TODO: remove randomState
data SnipsEnv = SnipsEnv { randomState :: TVar [Int16], names :: String, snippetPath :: String }

type SnipsNvim a = Neovim SnipsEnv a
