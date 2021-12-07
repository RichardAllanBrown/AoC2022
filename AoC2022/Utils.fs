module Utils

open System.IO

let readInputFile(fileName: string): string list =
    File.ReadAllLines(fileName)
    |> List.ofSeq

let splitDigits(s: string): int list =
    s.Split(',')
    |> Array.map(int)
    |> Array.toList
