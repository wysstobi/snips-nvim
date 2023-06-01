module Plugin.Types (Quotes, Snippet(..), Placeholder(..), PlaceholderState(..), PlaceholderST, SnipsEnv(..), SnipsNvim, SnippetMetaData(..)) where
import Control.Monad.Trans.State (StateT)
import Neovim

-- Environment
data SnipsEnv = SnipsEnv { names :: String, snippetPath :: String, qs :: Quotes }

newtype SnippetMetaData = SnippetMetaData { fileType :: String } deriving Show
instance Eq SnippetMetaData where
  (SnippetMetaData ft1) == (SnippetMetaData ft2) = ft1 == ft2

type SnipsNvim a = Neovim SnipsEnv a
data Snippet = Snippet { name :: String, content :: [String], meta :: SnippetMetaData } deriving Show

instance Eq Snippet where
  (Snippet n1 c1 m1) == (Snippet n2 c2 m2) = n1 == n2 && c1 == c2  && m1 == m2

type Quotes = (String, String)
-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving (Show, Eq)

-- instance Eq Placeholder where
--   (Placeholder k v) == (Placeholder k2 v2) = k == k2 && v == v2

data PlaceholderState = PS { snippet :: Snippet, quotes :: Quotes, placeholders :: [Placeholder] } deriving (Show, Eq)


type PlaceholderST a = StateT PlaceholderState (Neovim SnipsEnv) a


