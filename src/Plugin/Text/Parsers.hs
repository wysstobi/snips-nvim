module Plugin.Text.Parsers where

import Control.Applicative
import Data.Char

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

-- | Parses text until the given string appears
-- if the string was not found, the whole text will be parsed
--parseUntil :: String -> Parser String
--parseUntil text = parseUntil' text ("", "") where
--  parseUntil' "" (res, _) = return res
--  parseUntil' (t:ext) (res, end) = do
--    hasNextChar <- hasNext
--    if not hasNextChar then return res
--      else do
--        c <- item
--        if c == t then
--          parseUntil' ext (res,end ++ [c])
--        else
--          parseUntil' text (res ++ end ++ [c], "")
--

-- | returns true if the string to parse has at least one character left
hasNext :: Parser Bool
hasNext = P (\inp -> case inp of
                      (c:cs) -> Just (True, c:cs)
                      _    -> Just (False,[]))
 

-- following code has been copied from lecture

newtype Parser a = P { parse :: String -> Maybe (a, String) }


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

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const Nothing)

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

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then pure c else empty


item :: Parser Char
item =  P (\inp -> case inp of
                      (c:cs) -> Just (c, cs)
                      _    -> Nothing)

string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs

digit :: Parser Char
digit = sat isDigit

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)


space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol ss = token (string ss)

