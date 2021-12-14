{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils (countOccurrences)
import Data.Ord (comparing)
import Data.Foldable (maximumBy, minimumBy)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO

type Translations = Map String String

template = "NBOKHVHOSVKSSBSVVBCS"

parseLine :: Translations -> Text -> Translations
parseLine translations line =
  case T.splitOn "->" line of
    [pair, translation] -> M.insert (T.unpack pair) (T.unpack translation) translations
    _ -> translations

applyRules :: Translations -> String -> String
applyRules _ [] =  []
applyRules _ [c] = [c]
applyRules translations (first:second:rest) =
  case (first:[second]) `M.lookup` translations of
    Just translation ->
      (first:translation) ++ applyRules translations (second:rest)

    Nothing -> ""

solve :: String -> Translations -> Int -> Int
solve template translations n = max - min
  where
    occurrences = countOccurrences $ iterate (applyRules translations) template !! n
    max = snd $ maximumBy (comparing snd) $ M.toList occurrences
    min = snd $ minimumBy (comparing snd) $ M.toList occurrences

main = do
  input <- T.lines <$> TIO.readFile "day14/input.txt"
  let translations = foldl parseLine M.empty input

  -- Part 1
  print $ solve template translations 10

  -- This is way too inefficient for part 2. See Python solution like we did for Day 6.
  print $ solve template translations 40
    