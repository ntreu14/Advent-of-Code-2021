open System.IO

let input = 
  File.ReadAllLines "input.txt" |> Seq.map int

let findCountOfIncreases xs =
  Seq.pairwise xs
  |> Seq.filter (fun (previous, current) -> current > previous)
  |> Seq.length

let part1 =
  findCountOfIncreases input

let part2 =
  input 
  |> Seq.windowed 3
  |> Seq.map Seq.sum
  |> findCountOfIncreases

printfn "%i" part1
printfn "%i" part2