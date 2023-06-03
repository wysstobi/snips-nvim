{-# LANGUAGE FlexibleContexts #-}

module Plugin.Text.Searching where

import Plugin.Types (Snippet(Snippet), Quotes, PlaceholderState(..), Placeholder(..) )
import Control.Applicative
import Plugin.Text.Parsers (parseUntil, Parser, parse)
import Data.List (group, sort)
import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Class (MonadState(..))

-- | Extracts all @Placeholder@ from the @Snippet@ in the state.
-- Duplicates will be ignored.
-- This is used to create a new @Snippet@.
extractPlaceholders :: (MonadState PlaceholderState m) => m [Placeholder]
extractPlaceholders = placeholderSetFromStrings <$> placeholdersInList

-- | Wirtes all @Placeholder@ from the @Snippet@ in the state, to the state.
placeholdersInList :: (MonadState PlaceholderState m) => m [String]
placeholdersInList = do
  PS (Snippet _ content _) _ _ <- get
  placeholdersInList' content [] where
    placeholdersInList' :: (MonadState PlaceholderState m) => [String] -> [String] ->  m [String]
    placeholdersInList' [] found = pure found
    placeholdersInList' (line:rest) found = do
      psInLine <- placeholdersInLine line
      placeholdersInList' rest (found ++ psInLine)

-- | Finds all @Placeholder@s in the current line.
placeholdersInLine :: (MonadState PlaceholderState m) 
                   => String  -- ^ the line to find the @Placeholder@s in it.
                   ->  m [String]
placeholdersInLine line = do
  PS _ qs _ <- get
  let parsed = parse (many $ parseSingle qs) line
  let res = case parsed of
                        Just (result, _) -> result
                        Nothing       -> []
  return res

-- | Finds the next @Placeholder@.
parseSingle :: Quotes -- ^ the quotes surrounding the @Placeholder@
            -> Parser String
parseSingle (start, end) = parseUntil start *> parseUntil end

-- | Removes duplicates from the given list and creats @Placholder@s out of it.
placeholderSetFromStrings :: [String] -- ^ a list with possible duplicates
                          -> [Placeholder]
placeholderSetFromStrings = map (`Placeholder` Nothing) . setFromList

-- | Creates a set from a list
setFromList :: Ord a => [a] -> [a]
setFromList = map head . group . sort

