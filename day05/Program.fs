open System.IO
open Utils

let getReplicateCount a b = 
  (abs <| a - b) + 1  

let getDiagonalCoordinate a b =
  if a < b then
    [a .. b]
  else
    [a .. -1 .. b]
  
let getCoordinates includeDiagonals (x1, y1) (x2, y2) =
  let isDiagonal = (abs <| x1-x2) = (abs <| y1-y2)
  
  if includeDiagonals && isDiagonal then
    List.zip (getDiagonalCoordinate x1 x2) (getDiagonalCoordinate y1 y2)

  elif x1 = x2 then
    List.zip (List.replicate (getReplicateCount y1 y2) x1) [min y1 y2 .. max y1 y2]

  elif y1 = y2 then
    List.zip [min x1 x2 .. max x1 x2] <| List.replicate (getReplicateCount x1 x2) y1 

  else 
    []

let parseLineToVents getCoordinatesFn vents (line: string) =
  match splitOnWithoutEmpties line ' ' with
  | [| x1; y1; "->"; x2; y2 |] -> 

    getCoordinatesFn (int x1, int y1) (int x2, int y2)
    |> List.fold (fun state coordinate -> updateMapWith ((+) 1) 1 coordinate state) vents

  | _ -> vents

let input = File.ReadAllLines "input.txt" 

let solve includeDiagonals =
  input
  |> Seq.fold (parseLineToVents <| getCoordinates includeDiagonals) Map.empty
  |> mapCountWhere (fun _ v -> v >= 2)

// Part 1
solve false |> printfn "%i"

// Part 2
solve true |> printfn "%i"