open System.IO
open Utils

let input = File.ReadAllLines "input.txt"

// Part 1
let toOnlyKnownSegments soughtCounts (line: string) =
    let isSoughtCount (output: string) = Seq.contains output.Length soughtCounts

    match splitOnWithoutEmpties line '|' with
    | [| _; outputs |] -> splitOnWithoutEmpties outputs ' ' |> Seq.filter isSoughtCount
    | other -> failwith $"Cannot parse from part 1: {other}, length: {Seq.length other}"

Seq.map (toOnlyKnownSegments [2; 4; 3; 7]) input 
|> Seq.sumBy Seq.length
|> printfn "%i"


// Part 2
let knownLengthToSegmentLookup segments knownLengths = // length => known segment
    segments
    |> Seq.map (fun (segment: string) -> (segment.Length, set segment))
    |> Seq.filter (fun (length, _) -> Seq.contains length knownLengths)
    |> Map.ofSeq

let knownLengthToDigitLookup = // length => known digit
    Map.ofList [(2, 1); (4, 4); (3, 7); (7, 8)]

let parseSignalPatternsAndOutputs (line: string) =
    match splitOnWithoutEmpties line '|' with
    | [| signalPatterns; outputs |] ->
        (splitOnWithoutEmpties signalPatterns ' ', splitOnWithoutEmpties outputs ' ') 

    | other ->
        failwith $"Cannot parse: {other}, length: {Seq.length other}"

let isIntersectionCountOf segment intersectioncount knownDigitSegment =
    Set.intersect (set segment) knownDigitSegment
    |> Set.count 
    |> (=) intersectioncount

let decodeSignals knownLengthToSegment decodedLookup (segment: string) =
    match segment.Length with

    // Known segements by length
    | segmentLength when Seq.contains segmentLength <| Map.keys knownLengthToDigitLookup ->
        let digit = Map.find segmentLength knownLengthToDigitLookup
        Map.add (set segment) digit decodedLookup

    // Length 5
    | 5 ->
        // We know 3 because it has 2 intersections with 1
        if Map.find 2 knownLengthToSegment |> isIntersectionCountOf segment 2 then
            Map.add (set segment) 3 decodedLookup
        
        // We know 5 because it has 3 intersections with 4
        elif Map.find 4 knownLengthToSegment |> isIntersectionCountOf segment 3 then
            Map.add (set segment) 5 decodedLookup

        // Otherwise 2 is the only remaining digit with length 5
        else 
            Map.add (set segment) 2 decodedLookup

    // Length 6 
    | 6 ->
        // We know 9 because it has 4 intersections with 4
        if Map.find 4 knownLengthToSegment |> isIntersectionCountOf segment 4 then
            Map.add (set segment) 9 decodedLookup

        // We know 0 because it has 2 intersections with 1
        elif Map.find 2 knownLengthToSegment |> isIntersectionCountOf segment 2 then
            Map.add (set segment) 0 decodedLookup

        // Otherwise 6 is the only remaining digit with length 6
        else 
            Map.add (set segment) 6 decodedLookup

    // Truly an exceptional case, we did something very wrong to get here...
    | otherLength -> failwith $"Some other length {otherLength} of {segment}"

let decodeSegment (wholeLine: string) =
    let segments, outputs = parseSignalPatternsAndOutputs wholeLine 
    let knownLengthToSegment = knownLengthToSegmentLookup segments [2; 4; 3; 7]
    let decodedValues = segments |> Seq.fold (decodeSignals knownLengthToSegment) Map.empty

    // Find the output value from the map we just made, combine it's digits, and convert it to an int
    outputs 
    |> Seq.map (fun output -> Map.find (set output) decodedValues |> string) 
    |> String.concat ""
    |> int

input
|> Seq.sumBy decodeSegment
|> printfn "%i"