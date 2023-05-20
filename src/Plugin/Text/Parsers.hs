module Plugin.Text.Parsers where

import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: String -> Maybe (a, String) }

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item 
  if p c then pure c else empty

-- analog
-- sat p = item >>= \c -> if p c then pure c else empty

item :: Parser Char
item =  P (\inp -> case inp of
                      (c:cs) -> Just (c, cs)
                      _    -> Nothing)

digit :: Parser Char
digit = sat isDigit

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

instance Functor Parser where
 -- fmap :: (a -> b) -> Parser a -> Parser b
 fmap g p = P (\inp -> case parse p inp of
                         Nothing      -> Nothing
                         Just (v,out) -> Just (g v, out))


instance Applicative Parser where
  -- pure :: a -> Parser a 
  pure v = P (\inp -> Just (v,inp))

  -- <*> :: Parser (a->b) -> Parser a -> Parser b
  pg <*> pa = P (\inp ->
    case parse pg inp of
      Nothing      -> Nothing
      Just (g,out) -> parse (fmap g pa) out)

string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp ->
    case parse p inp of
      Nothing      -> parse q inp
      Just (v,out) -> Just (v,out))

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
    Nothing    -> Nothing
    Just (a, rest) -> parse (f a) rest)

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol ss = token (string ss)


parseUntil :: String -> Parser String 
parseUntil text = parseUntil' text ("", "") where
  parseUntil' "" (res, _) = return res
  parseUntil' (t:ext) (res, end) = do
    c <- item
    let result = res ++ [c]
    if c == t then 
      parseUntil' ext (res,end ++ [c])
    else 
      parseUntil' text (res ++ end ++ [c], "")

