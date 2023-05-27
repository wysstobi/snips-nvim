module Plugin.Types (Quotes, Snippet(..), Placeholder(..), PlaceholderState(..), PlaceholderST, SnipsEnv(..), SnipsNvim, SnippetMetaData(..)) where
import Control.Monad.Trans.State (StateT)
import Neovim

-- Environment
data SnipsEnv = SnipsEnv { names :: String, snippetPath :: String, qs :: Quotes }

newtype SnippetMetaData = SnippetMetaData { fileType :: String } deriving Show

type SnipsNvim a = Neovim SnipsEnv a
data Snippet = Snippet { name :: String, content :: [String], meta :: SnippetMetaData } deriving Show

type Quotes = (String, String)
-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving Show

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving Show


type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a




