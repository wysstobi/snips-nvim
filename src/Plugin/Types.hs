module Plugin.Types (Quotes, Snippet(..), Placeholder(..), PlaceholderState(..), PlaceholderST, PlaceholderSTOld, SnipsEnv(..), SnipsNvim) where
import Control.Monad.Trans.State (StateT)
import Neovim
import Control.Monad.ST.Lazy (ST)
import GHC.Conc (TVar)

-- Environment
type Quotes = (String, String)
-- TODO: remove randomState
data SnipsEnv = SnipsEnv { randomState :: TVar [Int16], names :: String, snippetPath :: String, qs :: Quotes }

type SnipsNvim a = Neovim SnipsEnv a
data Snippet = Snippet { name :: String, content :: [String] } deriving Show

-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving Show

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving Show

type PlaceholderSTOld a  = ST PlaceholderState a
type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a

