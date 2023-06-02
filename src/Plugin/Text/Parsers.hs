{-| This modules provides parser to search and replace placeholders in strings. -}
module Plugin.Text.Parsers where

import Control.Applicative

-- | Parses the text until the given string has been detected
-- The String will then be removed.
parseUntil :: String -- ^ the text will be parsed until this string occurs.
           -> Parser String 
parseUntil text = parseUntil' text ("", "") where
  parseUntil' "" (res, _) = return res
  parseUntil' (t:ext) (res, end) = do
    c <- item
    if c == t then 
      parseUntil' ext (res,end ++ [c])
    else 
      if c == head text then
        parseUntil' (tail text) (res ++ end, [c])
      else 
        parseUntil' text (res ++ end ++ [c], "")


-- following code has been copied from lecture

-- | Parses any char
item :: Parser Char
item =  P (\inp -> case inp of
                      (c:cs) -> Just (c, cs)
                      _    -> Nothing)

-- | Represents a parser
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

