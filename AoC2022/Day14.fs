module Day14

open InputParsers

type InsertionRule =
    { a: char; b: char; r: char }

type PolymerChain = char list

module PolymerChain =
    let computeScore(chain: PolymerChain): int =
        let counts = chain |> List.groupBy(id) |> List.map(fun (_, l) -> l.Length)
        List.max(counts) - List.min(counts)

let step(chain: PolymerChain)(rules: InsertionRule list): PolymerChain =
    let insertionsToDo = 
        [ for i in 0..chain.Length-2 do
                rules 
                |> List.tryFind(fun r -> r.a = chain[i] && r.b = chain[i+1])
                |> Option.map(fun r -> (i, r.r))
        ] 
        |> List.collect(Option.toList)
        |> Map
    
    chain 
    |> List.indexed
    |> List.collect(fun (i, p) -> if (insertionsToDo.ContainsKey(i)) then [ p; insertionsToDo.Item(i) ] else [ p ])

let parseFile(lines: string list): PolymerChain * InsertionRule list =
    let chain = lines.Head.ToCharArray() |> Array.toList
    let rules = 
        lines.Tail
        |> List.collect(function
            | ParseRegex "(.)(.) -> (.)" [ Char a; Char b; Char r ] -> List.singleton { a = a; b = b; r = r }
            | _ -> List.empty)
    (chain, rules)

let computeChainScore(steps: int)(chain: PolymerChain)(rules: InsertionRule list): int =
    [1 .. steps]
    |> List.fold(fun c -> fun _ -> step(c)(rules))(chain)
    |> PolymerChain.computeScore

let computeDay14Part1(): int =
    let chain, rules = Utils.readInputFile("./Input/Day14.txt") |> parseFile
    computeChainScore(10)(chain)(rules)

// TODO: Make more efficient, we only care about the counts, so perhaps use a recursive approach to percolate up counts when we reach only leaf nodes?
let computeDay14Part2(): int =
    let chain, rules = Utils.readInputFile("./Input/Day14.txt") |> parseFile
    computeChainScore(40)(chain)(rules)
