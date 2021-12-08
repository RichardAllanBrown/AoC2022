module Utils

open System.IO

let readInputFile(fileName: string): string list =
    File.ReadAllLines(fileName)
    |> List.ofSeq

let splitDigits(s: string): int list =
    s.Split(',')
    |> Array.map(int)
    |> Array.toList

let permute list =
    let rec inserts e = function
    | [] -> [[e]]
    | x::xs as list -> (e::list)::(inserts e xs |> List.map (fun xs' -> x::xs'))
    
    List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list
    
    
