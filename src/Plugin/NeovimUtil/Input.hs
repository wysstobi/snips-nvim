module Plugin.NeovimUtil.Input ( askForString ) where

import Neovim.API.String
import Neovim
import Plugin.Types (SnipsNvim)
import qualified Data.Maybe

-- These functions are taken from https://github.com/neovimhaskell/nvim-hs-contrib


-- | Helper function that calls the @input()@ function of neovim.
input :: NvimObject result
      => String -- ^ Message to display
      -> Maybe String -- ^ Input fiiled in
      -> Maybe String -- ^ Completion mode
      -> SnipsNvim result
input message mPrefilled mCompletion = fmap fromObjectUnsafe
  $ vim_call_function "input" $ (message <> " ")
    +: Data.Maybe.fromMaybe "" mPrefilled
    +: maybe [] (+: []) mCompletion

-- | Helper function to get a tet input from the user
askForString :: String -- ^ message to put in front
             -> Maybe String -- ^ Prefilled text
             -> SnipsNvim String
askForString message mPrefilled = input message mPrefilled Nothing

