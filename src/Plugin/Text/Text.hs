{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use execState" #-}

module Plugin.Text.Text where

import Plugin.Types (Snippet(Snippet), PlaceholderT, modify, Placeholder(..))
import Data.Char (isDigit)
import GHC.Unicode (isAlphaNum)
import Control.Applicative
import Plugin.Text.Parsers (sat, string, parseUntil, Parser, parse)
import Data.List (group, sort)
import Data.Maybe(fromMaybe)

type Quotes = (String, String)

extractPlaceholders :: Snippet -> Quotes -> [Placeholder]
extractPlaceholders (Snippet _ content) =
   map (`Placeholder` Nothing) . rmdups . placeholdersInList content
  -- modify (\_ -> map (`Placeholder` Nothing) placeholders)
  -- pure $ mconcat content

placeholdersInList :: [String] -> Quotes -> [String]
placeholdersInList lines quotes = placeholdersInList' lines [] where
  placeholdersInList' (a:as) found = found ++ placeholdersInLine a quotes ++ placeholdersInList' as found
  placeholdersInList' [] found = found


placeholdersInLine :: String -> Quotes -> [String]
placeholdersInLine line quotes = case parseLine quotes line of
                        Just (res, _) -> res
                        Nothing       -> []

parseLine :: Quotes -> String -> Maybe ([String], String)
parseLine quotes = parse (many $ parseSingle quotes)


parseSingle :: Quotes -> Parser String
parseSingle (start, end) = parseUntil start *> parseUntil end

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


replaceNext :: [Placeholder] -> Quotes -> Parser String
replaceNext placeholders (start, end) = do
  before <- parseUntil start
  found <- parseUntil end
  let replacement = getReplacementForKey placeholders found
  return (before ++ replacement)

replaceInLine :: [Placeholder] -> Quotes -> String -> Maybe String
replaceInLine placeholders quotes line = mconcat . fst <$> parse (many (replaceNext placeholders quotes)) line

replaceInText :: [Placeholder] -> [String] -> Quotes -> Maybe [String]
replaceInText placeholders [] _ =  Just []
replaceInText placeholders (l:ine) quotes = case replaceInLine placeholders quotes l of
    Just str -> (++) <$> Just [str] <*> replaceInText placeholders ine quotes

getReplacementForKey :: [Placeholder] -> String -> String
getReplacementForKey [] _ = ""
getReplacementForKey ((Placeholder key value):ps) placeholder =
  if placeholder == key
    then
      fromMaybe "" value
    else
      getReplacementForKey ps placeholder


