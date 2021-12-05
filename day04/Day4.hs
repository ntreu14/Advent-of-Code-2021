module Day04 where

import Utils
import Data.List (groupBy, find, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

numsCalled :: [Int]
numsCalled = [74,79,46,2,19,27,31,90,21,83,94,77,0,29,38,72,42,23,6,62,45,95,41,55,93,69,39,17,12,1,20,53,49,71,61,13,88,25,87,26,50,58,28,51,89,64,3,80,36,65,57,92,52,86,98,78,9,33,44,63,16,34,97,60,40,66,75,4,7,84,22,43,11,85,91,32,48,14,18,76,8,47,24,81,35,30,82,67,37,70,15,5,73,59,54,68,56,96,99,10]

data BoardSpace
  = Marked Int
  | Unmarked Int
  deriving Eq

data Winner
  = NoWinnerYet
  | Winner Board
  
type Board = Map Coordinate BoardSpace

parseBoards :: [String] -> [Board]
parseBoards = trd . foldl makeMapPerBoard (M.empty, 0, [])
  where
    mapOfRow rowNum columnNums = M.fromList $ do
      (x, n) <- zip [0 ..] columnNums
      pure ((x, rowNum), Unmarked n)

    makeMapPerBoard (runningBoard, rowNumber, allBoards) currentLine =
      case words currentLine of
        ["--"] -> (M.empty, 0, runningBoard : allBoards)

        boardNums ->
          (M.union runningBoard currentRowMap, rowNumber + 1, allBoards)
          where
            currentRowMap = mapOfRow rowNumber $ map read boardNums

detectBingo :: Board -> Bool
detectBingo board = isRowBingo || isColumnBingo
  where
    isBingo = all (\coord -> isCoordinateMarked $ M.lookup coord board)

    isCoordinateMarked (Just (Marked _)) = True
    isCoordinateMarked _                 = False

    isRowBingo = any isBingo byRows
      where
        byRows = groupBy (\(_, y) (_, y') -> y == y') $ do
          -- we know the board is 5 x 5
          row <- [0..4]
          col <- [0..4]
          pure (col, row)

    isColumnBingo = any isBingo byColumns
      where
        byColumns = groupBy (\(x, _) (x', _) -> x == x') $ do
          -- we know the board is 5 x 5
          col <- [0..4]
          row <- [0..4]
          pure (col, row)

sumUnmarkedSpaces :: BoardSpace -> Int -> Int
sumUnmarkedSpaces (Unmarked n) lastSpace = n + lastSpace
sumUnmarkedSpaces _ lastSpace            = lastSpace

playRound :: Int -> [Board] -> [Board]
playRound numCalled = map $ M.map markNumberCalled
  where
    markNumberCalled (Unmarked n)
      | n == numCalled = Marked n
      | otherwise      = Unmarked n
    markNumberCalled boardSpace = boardSpace

playBingoGameUntilWinner :: [Board] -> [Int] -> Int
playBingoGameUntilWinner _ [] = 0
playBingoGameUntilWinner boards (numCalled:remainingNums) =
  case maybeFindWinner afterRound of
    NoWinnerYet -> playBingoGameUntilWinner afterRound remainingNums
    Winner winner -> numCalled * M.foldr sumUnmarkedSpaces 0 winner
  
  where
    afterRound = playRound numCalled boards
    maybeFindWinner = maybe NoWinnerYet Winner . find detectBingo

playBingoGameUntilLastWinner :: [Board] -> [Board] -> [Int] -> Int -> Int
playBingoGameUntilLastWinner [] previousWinners _ previousLastCalled =
  previousLastCalled * M.foldr sumUnmarkedSpaces 0 (last previousWinners)

playBingoGameUntilLastWinner boards previousWinners (numCalled:remainingNums) _ =
  case foundWinners of
    [] -> playBingoGameUntilLastWinner afterRound previousWinners remainingNums numCalled
    
    winners ->
      playBingoGameUntilLastWinner (nextBoardsWithoutWinners winners) winners remainingNums numCalled

  where
    afterRound = playRound numCalled boards
    foundWinners = filter detectBingo afterRound
    nextBoardsWithoutWinners winners = afterRound \\ winners

main :: IO ()
main = do
  input <- readFileLines "day04/input.txt"  
  let bingoBoards = parseBoards input

  -- Part 1
  print $ playBingoGameUntilWinner bingoBoards numsCalled

  -- Part 2
  print $ playBingoGameUntilLastWinner bingoBoards [] numsCalled 0
