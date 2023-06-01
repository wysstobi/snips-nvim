{-# LANGUAGE FlexibleContexts #-}

module Plugin.Text.Text where

import Plugin.Types (Snippet(Snippet), Quotes, PlaceholderState(..), Placeholder(..) )
import Control.Applicative
import Plugin.Text.Parsers (parseUntil, Parser, parse)
import Data.List (group, sort)
import Data.Maybe(fromMaybe)

import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Class (MonadState(..))

-- | find placeholders
extractPlaceholders :: (MonadState PlaceholderState m) => m [Placeholder]
extractPlaceholders = placeholderSetFromStrings <$> placeholdersInList

-- | Gets all placeholders from the text in the state.
placeholdersInList :: (MonadState PlaceholderState m) => m [String]
placeholdersInList = do
  PS (Snippet _ content _) _ _ <- get
  placeholdersInList' content [] where
    placeholdersInList' :: (MonadState PlaceholderState m) => [String] -> [String] ->  m [String]
    placeholdersInList' [] found = pure found
    placeholdersInList' (line:rest) found = do
      psInLine <- placeholdersInLine line
      placeholdersInList' rest (found ++ psInLine)


-- | Gets all placeholders in the current line.
placeholdersInLine :: (MonadState PlaceholderState m) => String ->  m [String]
placeholdersInLine line = do
  PS _ qs _ <- get
  let parsed = parse (many $ parseSingle qs) line
  let res = case parsed of
                        Just (result, _) -> result
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
  PS (Snippet _ content _) _ _ <- get
  replaceInText' content where
    replaceInText' [] = pure $ Just []
    replaceInText' (line:rest) = do
      currentLine  <- replaceInLine line
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
  PS _ qs placeholders <- get
  let parsed = parse (many (replaceNext placeholders qs)) currentLine
  let replaced = mconcat . fst <$> parsed
  let rest = snd <$> parsed
  return $ (++) <$> replaced <*> rest

getReplacementForKey :: [Placeholder] -> String -> String
getReplacementForKey [] _ = ""
getReplacementForKey ((Placeholder key value):ps) placeholder =
  if placeholder == key
    then
      fromMaybe "" value
    else
      getReplacementForKey ps placeholder

