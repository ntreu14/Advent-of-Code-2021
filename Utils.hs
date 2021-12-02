module Utils
  ( Coordinate
  , readFileLines
  , toCoordinateMap    
  )
  where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coordinate = (Int, Int)

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName

toCoordinateMap :: [[a]] -> Map Coordinate a
toCoordinateMap xs = M.fromList $ do
  (y, row) <- zip [0 ..] xs
  (x, v) <- zip [0 ..] row
  pure ((x, y), v)
