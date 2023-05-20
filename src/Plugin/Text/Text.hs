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


-- find placeholders
extractPlaceholders :: PlaceholderST [Placeholder]
extractPlaceholders = do
   placeholders <- placeholdersInList []
   return $ map (`Placeholder` Nothing) . rmdups $ placeholders

placeholdersInList :: [String] -> PlaceholderST [String]
placeholdersInList found = do
  PS (Snippet name content) qs placeholders <- get
  if null content then
    return found
  else do
    psInLine <- placeholdersInLine
    PS s qs placeholders <- get
    psInCurrentline <- placeholdersInLine
    placeholdersInList (found ++ psInCurrentline)

placeholdersInLine :: PlaceholderST [String]
placeholdersInLine = do 
  PS (Snippet name content) qs placeholders <- get
  put $ PS (Snippet name $ tail content) qs placeholders
  let res = case parseLine qs (head content) of
                        Just (res, _) -> res
                        Nothing       -> []
  return res

parseLine :: Quotes -> String -> Maybe ([String], String)
parseLine quotes = parse (many $ parseSingle quotes)

parseSingle :: Quotes -> Parser String
parseSingle (start, end) = parseUntil start *> parseUntil end

rmdups ::  [String] -> [String]
rmdups = map head . group . sort





-- replace

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


