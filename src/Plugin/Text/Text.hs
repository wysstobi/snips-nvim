{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use execState" #-}

module Plugin.Text.Text where

import Plugin.Types (Snippet(Snippet), PlaceHolderT, modify, PlaceHolder(..))
import Data.Char (isDigit)
import GHC.Unicode (isAlphaNum)
import Control.Applicative
import Plugin.Text.Parsers (sat, string, parseUntil, Parser, parse)
import Data.List (group, sort)

type Quotes = (String, String)

extractPlaceHolders :: Snippet -> Quotes -> [PlaceHolder]
extractPlaceHolders (Snippet _ content) =
   map (`PlaceHolder` Nothing) . rmdups . placeHoldersInList content
  -- modify (\_ -> map (`PlaceHolder` Nothing) placeHolders)
  -- pure $ mconcat content

placeHoldersInList :: [String] -> Quotes -> [String]
placeHoldersInList lines quotes = placeHoldersInList' lines [] where
  placeHoldersInList' (a:as) found = found ++ placeHoldersInLine a quotes ++ placeHoldersInList' as found
  placeHoldersInList' [] found = found


placeHoldersInLine :: String -> Quotes -> [String]
placeHoldersInLine line quotes = case parseLine quotes line of
                        Just (res, _) -> res
                        Nothing       -> []

parseLine :: Quotes -> String -> Maybe ([String], String)
parseLine quotes = parse (many $ parseSingle quotes)


parseSingle :: Quotes -> Parser String
parseSingle (start, end) = parseUntil start *> parseUntil end

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
