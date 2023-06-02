{-# LANGUAGE FlexibleContexts #-}
module Plugin.Text.Replacing (replaceInText) where

import Plugin.Types (Snippet(Snippet), Quotes, PlaceholderState(..), Placeholder(..) )
import Control.Applicative
import Plugin.Text.Parsers (parseUntil, Parser, parse)
import Data.Maybe(fromMaybe)

import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Class (MonadState(..))

-- | Replaces all @Placeholder@ from the @Snippet@ with the according value.
-- This is used to insert an existing @Snippet@ into the code.
replaceInText :: (MonadState PlaceholderState m) => m (Maybe [String])
replaceInText = do
  PS (Snippet _ content _) _ _ <- get
  replaceInText' content where
    replaceInText' [] = pure $ Just []
    replaceInText' (line:rest) = do
      currentLine  <- replaceInLine line
      replacedRest <- replaceInText' rest
      pure $ (:) <$> currentLine <*> replacedRest

-- | Replaces the next @Placeholder@ with its value.
replaceNext :: [Placeholder] -- ^ all existing @Placeholder@s with their values
            -> Quotes -> Parser String
replaceNext placeholders (start, end) = do
  before <- parseUntil start
  found  <- parseUntil end
  let replacement = getReplacementForKey placeholders found
  return (before ++ replacement)

-- | Replaces all @Placeholder@s in a given line
replaceInLine :: (MonadState PlaceholderState m) 
              => String -- ^ the line to replace the @Placeholder@s in
              -> m (Maybe String)
replaceInLine currentLine = do
  PS _ qs placeholders <- get
  let parsed = parse (many (replaceNext placeholders qs)) currentLine
  let replaced = mconcat . fst <$> parsed
  let rest = snd <$> parsed
  return $ (++) <$> replaced <*> rest

-- | Finds the matching replacement for a key. 
-- If the key or its corresponding @Placholder@ does not exist, an empty @String@ 
-- will be returned.
getReplacementForKey :: [Placeholder] -- ^ all existing placheolders
                     -> String -- ^ the key to get the replacement for
                     -> String
getReplacementForKey [] _ = ""
getReplacementForKey ((Placeholder key value):ps) placeholder =
  if placeholder == key
    then
      fromMaybe "" value
    else
      getReplacementForKey ps placeholder

