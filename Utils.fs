module Utils

let updateMapWith fn newValue key map =
  match Map.tryFind key map with
  | Some oldValue -> Map.add key (fn oldValue) map
  | None -> Map.add key newValue map