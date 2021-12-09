module Main where

import Utils
    ( getAdjacentPoints,
      readFileLines,
      toCoordinateMap,
      toInts,
      Coordinate,
      getAdjacentCoordinates )
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Grid = Map Coordinate Int

findLowPoints :: Grid -> Grid
findLowPoints grid = M.filterWithKey isLowerThanAdjacents grid
  where
    isLowerThanAdjacents coord point = all (point <) $ getAdjacentPoints coord grid

floodBasin :: Coordinate -> Grid -> Set Coordinate -> Set Coordinate 
floodBasin basinCoordinate grid alreadyFound
  | S.null neighbors = alreadyFound
  | otherwise = 
    S.unions $ S.map (\c -> floodBasin c grid (S.union neighbors $ S.insert basinCoordinate alreadyFound)) neighbors

  where
    adjCoordinates = getAdjacentCoordinates basinCoordinate grid
    currentPoint = basinCoordinate `M.lookup` grid

    isNeighbor coord point  =
      coord `elem` adjCoordinates && point < 9 && Just point > currentPoint && coord `S.notMember` alreadyFound

    neighbors = S.fromList $ M.keys $ M.filterWithKey isNeighbor grid

part1 = sum . map (+ 1) . M.elems . findLowPoints

part2 grid = product $ take 3 $ sortOn negate basinSizes
  where
    lowPoints = M.keys $ findLowPoints grid
    basinSizes = map (\lowPoint -> S.size $ floodBasin lowPoint grid S.empty) lowPoints

main = do
  input <- toInts <$> readFileLines "day09/input.txt"
  let grid = toCoordinateMap input

  -- Part 1
  print $ part1 grid

  -- Part 2, awful performance wise but works.
  -- Compiling instead of `runghc` runs faster
  print $ part2 grid