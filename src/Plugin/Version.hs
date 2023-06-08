module Plugin.Version (snipsVersion) where

import Plugin.Types (SnipsNvim)

-- | The version of the @SnipsNvim@ plugin
snipsVersion :: SnipsNvim String
snipsVersion = pure "0.1.0.0"
