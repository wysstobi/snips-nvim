{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use execState" #-}

module Plugin.Text.Text where

import Plugin.Types (Snippet(Snippet), PlaceholderST(..), Quotes, modify, PlaceholderState(..), Placeholder(..), get, put)
import Data.Char (isDigit)
import GHC.Unicode (isAlphaNum)
import Control.Applicative
import Plugin.Text.Parsers (sat, string, parseUntil, Parser, parse)
import Data.List (group, sort)
import Data.Maybe(fromMaybe)


-- | find placeholders
extractPlaceholders :: PlaceholderST [Placeholder]
extractPlaceholders = placeholderSetFromStrings <$> placeholdersInList []

-- | Gets all placeholders from the list in the state.
placeholdersInList :: [String] -> PlaceholderST [String]
placeholdersInList found = do
  PS (Snippet name content) qs placeholders <- get
  -- if content is empty, we are done
  if null content then
    return found
  else do
    -- save the rest of the content in the state
    put $ PS (Snippet name $ tail content) qs placeholders
    -- search the first line for placeholders
    psInLine <- placeholdersInLine (head content)
    placeholdersInList (found ++ psInLine)

-- | Gets all placeholders in the current line.
placeholdersInLine :: String -> PlaceholderST [String]
placeholdersInLine line = do
  PS (Snippet name content) qs placeholders <- get
  let parsed = parse (many $ parseSingle qs) line
  let res = case parsed of
                        Just (res, _) -> res
                        Nothing       -> []
  return res

-- | Parse a line and gets all placeholders in it.
parseSingle :: Quotes -> Parser String
parseSingle (start, end) = parseUntil start *> parseUntil end

placeholderSetFromStrings :: [String] -> [Placeholder]
placeholderSetFromStrings = map (`Placeholder` Nothing) . setFromList

setFromList :: Ord a => [a] -> [a]
setFromList = map head . group . sort

-- replace
replaceInText :: PlaceholderST (Maybe [String])
replaceInText = do
  PS (Snippet _ content) _ _ <- get
  -- if content is empty, we are done
  if null content then pure $ Just []
  else do
    currentLine <- replaceInLine
    rest        <- replaceInText
    pure $ (:) <$> currentLine <*> rest


replaceNext :: [Placeholder] -> Quotes -> Parser String
replaceNext placeholders (start, end) = do
  before <- parseUntil start
  found  <- parseUntil end
  let replacement = getReplacementForKey placeholders found
  return (before ++ replacement)

replaceInLine :: PlaceholderST (Maybe String)
replaceInLine = do
  PS (Snippet name content) qs placeholders <- get
  put $ PS (Snippet name (tail content)) qs placeholders
  let parsed = parse (many (replaceNext placeholders qs)) $ head content
  pure (mconcat . fst <$> parsed)

getReplacementForKey :: [Placeholder] -> String -> String
getReplacementForKey [] _ = ""
getReplacementForKey ((Placeholder key value):ps) placeholder =
  if placeholder == key
    then
      fromMaybe "" value
    else
      getReplacementForKey ps placeholder


-- TODO: remove or use
replaceNext2 :: PlaceholderST [Placeholder]
replaceNext2 = do
  PS (Snippet _ content) (start, end) placeholders <- get
  let found = parse (parseUntil start *> parseUntil end) (head content)
  return []
