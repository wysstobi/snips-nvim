module Plugin.Types (Quotes, Snippet(..), Placeholder(..), PlaceholderState(..), PlaceholderST, PlaceholderSTOld) where
import Control.Monad.Trans.State (StateT)
import Plugin.Environment.SnipsEnvironment (SnipsNvim, SnipsEnv)
import Neovim
import Control.Monad.ST.Lazy (ST)

data Snippet = Snippet { name :: String, content :: [String] } deriving Show

-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving Show
type Quotes = (String, String)

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving Show

type PlaceholderSTOld a  = ST PlaceholderState a
type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a

