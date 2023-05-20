{-# LANGUAGE TupleSections #-}

module Plugin.Types (Quotes, Snippet(..), Placeholder(..), PlaceholderST, modify, get, put, runState) where

data Snippet = Snippet { name :: String, content :: [String] }



-- State
data Placeholder = Placeholder { key :: String, value :: Maybe String } deriving Show
type Quotes = (String, String)
data PlaceholderState = PS { quotes :: Quotes, placeHolders :: [Placeholder] } deriving Show

type PlaceholderST = ST PlaceholderState String

newtype ST s a = S (s -> (a, s))
runState :: ST s a -> s -> (a, s)
runState (S t) = t

instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST s a -> ST s b
  fmap f (S fa) = S (\s ->
    case fa s of
      (a,s') -> (f a, s'))

instance Applicative (ST s) where
  -- pure :: a -> ST s a 
  pure a = S (a,)
  -- (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  stf <*> sta = do
    f <- stf
    f <$> sta

instance Monad (ST s) where
  -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  (S s1) >>= f = S (\s -> case s1 s of
      (a, s') -> runState (f a) s')

-- Reads the state
get :: ST s s
get = S (\s -> (s,s))

-- Writes the state
put :: s -> ST s ()
put w = S (const ((), w))

modify :: (s -> s) -> ST s ()
modify f = do
  w <- get
  put (f w)

