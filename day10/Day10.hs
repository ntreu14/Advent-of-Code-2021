module Day10 where

import Utils (readFileLines)
import Data.List (sort)

isChunkClosed '(' ')' = True
isChunkClosed '[' ']' = True
isChunkClosed '{' '}' = True
isChunkClosed '<' '>' = True
isChunkClosed _ _     = False

maybeIllegalChar = go []
  where
    go stack [] = (stack, Nothing)
    go stack (c:rest) | c `elem` ['(','[','{','<'] = go (c:stack) rest
    go (s:restStack) (c:rest) | isChunkClosed s c = go restStack rest
    go stack (c:_) = (stack, Just c)

getIllegalScore line =
  case snd $ maybeIllegalChar line of
    Just ')' -> 3
    Just ']' -> 57
    Just '}' -> 1197
    Just '>' -> 25137
    _        -> 0

getAutocompleteCharPoints = go 0
  where
    go score [] = score
    go score ('(':xs) = go (1 + score * 5) xs
    go score ('[':xs) = go (2 + score * 5) xs
    go score ('{':xs) = go (3 + score * 5) xs
    go score ('<':xs) = go (4 + score * 5) xs

getAutocompletePoints line =
  case maybeIllegalChar line of
    (_, Just _) -> 0
    ([], Nothing) -> 0
    (s, Nothing) -> getAutocompleteCharPoints s

part1 = sum . map getIllegalScore

part2 input = scores !! (length scores `div` 2)
  where
    scores = sort $ filter (> 0) $ map getAutocompletePoints input

main = do
  input <- readFileLines "day10/input.txt"

  -- Part 1
  print $ part1 input

  -- Part 2
  print $ part2 input