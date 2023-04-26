import Neovim

import qualified Plugin as SnippetPlugin

main :: IO ()
main = do
    neovim defaultConfig
        { plugins = plugins defaultConfig ++ [ SnippetPlugin.plugin ]
        }
