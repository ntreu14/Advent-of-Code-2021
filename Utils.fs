module Utils

open System

let updateMapWith fn newValue key map =
  match Map.tryFind key map with
  | Some oldValue -> Map.add key (fn oldValue) map
  | None -> Map.add key newValue map

let mapCountWhere predicate =
  Map.count << Map.filter predicate

let splitOnWithoutEmpties (str: string) (delimeter: char) =
  str.Split (delimeter, StringSplitOptions.RemoveEmptyEntries)