module Plugin.Environment.SnipsEnvironment where

import GHC.Conc
import Data.Int
import Neovim
--import Plugin.Types (Quotes)

-- move this module to types
type Quotes = (String, String)
-- TODO: remove randomState
--
data SnipsEnv = SnipsEnv { randomState :: TVar [Int16], names :: String, snippetPath :: String, quotes :: Quotes }

type SnipsNvim a = Neovim SnipsEnv a
