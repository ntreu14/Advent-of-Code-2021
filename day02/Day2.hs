module Day2 where

import Utils (readFileLines)

data SubmarinePosition = SubmarinePosition
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  }

runCommandPart1 :: SubmarinePosition -> String -> SubmarinePosition
runCommandPart1 sp@SubmarinePosition{ horizontal=lastHorizontal, depth=lastDepth } command =
  case words command of
    ["forward", x] -> 
      sp { horizontal=lastHorizontal + read x }

    ["down", x] -> 
      sp { depth=lastDepth + read x }
    
    ["up", x] -> 
      sp { depth=lastDepth - read x }

    _ -> sp 

runCommandPart2 :: SubmarinePosition -> String -> SubmarinePosition
runCommandPart2 sp@SubmarinePosition{ horizontal=lastHorizontal, depth=lastDepth, aim=lastAim } command =
  case words command of
    ["forward", x] ->
      sp { horizontal=lastHorizontal + read x, depth=lastDepth + lastAim * read x }

    ["down", x] -> 
      sp { aim=lastAim + read x }
    
    ["up", x] -> 
      sp { aim=lastAim - read x }

    _ -> sp

runCommands :: (SubmarinePosition -> String -> SubmarinePosition) -> [String] -> Int
runCommands commandFn input =
  horizontal finalSubmarinePosition * depth finalSubmarinePosition
  where 
    finalSubmarinePosition = foldl commandFn (SubmarinePosition 0 0 0) input

main :: IO ()
main = do
  input <- readFileLines "day02/input.txt"

  -- Part 1
  print $ runCommands runCommandPart1 input

  -- Part 2
  print $ runCommands runCommandPart2 input