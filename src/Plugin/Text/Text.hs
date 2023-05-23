{-# LANGUAGE FlexibleContexts #-}

module Plugin.Text.Text where

import Plugin.Types (Snippet(Snippet), PlaceholderST(..), Quotes, PlaceholderState(..), Placeholder(..) )
import Data.Char (isDigit)
import GHC.Unicode (isAlphaNum)
import Control.Applicative
import Plugin.Text.Parsers (sat, string, parseUntil, Parser, parse)
import Data.List (group, sort)
import Data.Maybe(fromMaybe)

--import Control.Monad.Trans.State (get, put)
import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Class (MonadState(..))

-- | find placeholders
extractPlaceholders :: (MonadState PlaceholderState m) => m [Placeholder]
extractPlaceholders = placeholderSetFromStrings <$> placeholdersInList

-- | Gets all placeholders from the text in the state.
placeholdersInList :: (MonadState PlaceholderState m) => m [String]
placeholdersInList = do
  PS (Snippet name content) qs placeholders <- get
  placeholdersInList' content [] where
    placeholdersInList' :: (MonadState PlaceholderState m) => [String] -> [String] ->  m [String]
    placeholdersInList' [] found = pure found
    placeholdersInList' (line:rest) found = do
      PS (Snippet name _) qs placeholders <- get
      psInLine <- placeholdersInLine line
      placeholdersInList' rest (found ++ psInLine)


-- | Gets all placeholders in the current line.
placeholdersInLine :: (MonadState PlaceholderState m) => String ->  m [String]
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

replaceInText :: (MonadState PlaceholderState m) => m (Maybe [String])
replaceInText = do
  PS (Snippet _ content) _ _ <- get
  replaceInText' content where
    replaceInText' [] = pure $ Just []
    replaceInText' (line:rest) = do
      currentLine <- replaceInLine line
      replacedRest <- replaceInText' rest
      pure $ (:) <$> currentLine <*> replacedRest

replaceNext :: [Placeholder] -> Quotes -> Parser String
replaceNext placeholders (start, end) = do
  before <- parseUntil start
  found  <- parseUntil end
  let replacement = getReplacementForKey placeholders found
  return (before ++ replacement)

replaceInLine :: (MonadState PlaceholderState m) => String -> m (Maybe String)
replaceInLine currentLine = do
  PS (Snippet name content) qs placeholders <- get
  let parsed = parse (many (replaceNext placeholders qs)) currentLine
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
