module SnipsLib(main) where

import Neovim
import qualified Plugin as Snips

main :: IO ()
main = do
    neovim defaultConfig
        { plugins = plugins defaultConfig ++ [ Snips.plugin ]
        }
