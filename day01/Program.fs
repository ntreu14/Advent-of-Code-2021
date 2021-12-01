open System.IO

let findIncreases (total, last) current =
    if current > last then
        (total + 1, current)
    else
        (total, current)

let findIncreaseSums (total, last) xs =
    let currentSum = Seq.sum xs

    if currentSum > last then
        (total + 1, currentSum)
    else
        (total, currentSum)

let input = 
    File.ReadAllLines "input.txt" |> Seq.map int

let part1 =
    input
    |> Seq.fold findIncreases (0, 150) // 150 is my first input value
    |> fst

let part2 =
    input 
    |> Seq.windowed 3
    |> Seq.fold findIncreaseSums (0, Seq.take 3 input |> Seq.sum) // Need to start with the sum of the first window
    |> fst

printfn "%i" part1
printfn "%i" part2