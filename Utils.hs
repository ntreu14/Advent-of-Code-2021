{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( Coordinate
  , readFileLines
  , toCoordinateMap
  , trd  
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coordinate = (Int, Int)

readFileLines :: FilePath -> IO [String]
readFileLines filePath = lines <$> readFile filePath

toCoordinateMap :: [[a]] -> Map Coordinate a
toCoordinateMap xs = M.fromList $ do
  (y, row) <- zip [0 ..] xs
  (x, v) <- zip [0 ..] row
  pure ((x, y), v)

trd :: (a, b, c) -> c
trd (_, _, c) = c