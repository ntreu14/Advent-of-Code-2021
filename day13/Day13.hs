{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import Control.Monad ((>=>))
import Utils (Coordinate, mapBoth)
import Data.Maybe (mapMaybe)
import Data.List (nub, partition)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Axis = X | Y
type AxisFold = (Axis, Int)

reflectPointOnAxis :: AxisFold -> Coordinate -> Coordinate
reflectPointOnAxis (Y, value) (x, y) = (x, y - (y-value) * 2)
reflectPointOnAxis (X, value) (x, y) = (x - (x-value) * 2, y)

performFold :: [Coordinate] -> AxisFold -> [Coordinate]
performFold coords (axis, value) =
  nub $ cordsLessThanAxis ++ (reflectPointOnAxis (axis, value) <$> coordsMoreThanAxis)
  where
    (cordsLessThanAxis, coordsMoreThanAxis) =
      case axis of
        X -> partition (\(x, _) -> x < value) coords
        Y -> partition (\(_, y) -> y < value) coords

part1 :: [Coordinate] -> [AxisFold] -> Int
part1 coords axisFolds = length $ performFold coords (head axisFolds) 

part2 :: [Coordinate] -> [AxisFold] -> String
part2 coords folds = drawCode $ foldl performFold coords folds 

drawCode :: [Coordinate] -> String 
drawCode coords = allCoords >>= drawPoint
  where
    minX = fst $ minimumBy (comparing fst) coords
    maxX = fst $ maximumBy (comparing fst) coords
    minY = snd $ minimumBy (comparing snd) coords
    maxY = snd $ maximumBy (comparing snd) coords

    allCoords = do
      y <- [minY .. maxY]
      x <- [minX .. maxX]
      pure (y, x)

    drawPoint (y, x) =
      -- Print "#" when the coord is found and "\n" when we reached the end of the line
      case ((x, y) `elem` coords, x == maxX) of
        (True, True) -> "#" ++ "\n"
        (True, False) -> "#" ++ " "
        (False, True) -> "." ++ "\n"
        (False, False) -> "." ++ " "

splitLineBy :: Text -> Text -> Maybe (Text, Text)
splitLineBy delimiter line =
  case T.splitOn delimiter line of
    [first, second] -> Just (first, second)
    _               -> Nothing

parseCoord :: (Text, Text) -> Coordinate
parseCoord = mapBoth (read . T.unpack)

parseFolds :: (Text, Text) -> Maybe AxisFold
parseFolds ("x", v) = Just (X, read $ T.unpack v)
parseFolds ("y", v) = Just (Y, read $ T.unpack v)
parseFolds _        = Nothing

main = do
  coords <- mapMaybe (fmap parseCoord . splitLineBy ",") . T.lines <$> TIO.readFile "day13/coords.txt"
  folds <- mapMaybe (splitLineBy "=" >=> parseFolds) . T.lines <$> TIO.readFile "day13/folds.txt"

  print $ part1 coords folds
  putStrLn $ part2 coords folds