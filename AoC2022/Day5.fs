module Day5

open Point

let getRange a b =
    if (a<b) then [a..b]
    else [b..a] |> List.rev

type Line = 
    { source: Point; dest: Point }

    static member isVerticalOrHorizontal(l: Line): bool =
        l.source.x = l.dest.x || l.source.y = l.dest.y

    static member isPerfectDiagonal(l: Line): bool =
        let absXDiff = abs(l.source.x - l.dest.x)
        let absYDiff = abs(l.source.y - l.dest.y)
        absXDiff = absYDiff && 0 < absXDiff

    member this.isVerticalOrHorizontal(): bool = Line.isVerticalOrHorizontal(this)

    member this.isPerfectDiagonal(): bool = Line.isPerfectDiagonal(this)

    member this.coversPoints(): Point Set =
        if (this.source.x = this.dest.x) then getRange this.source.y this.dest.y |> List.map(fun y -> { x = this.source.x; y = y }) |> Set
        else if (this.source.y = this.dest.y) then getRange this.source.x this.dest.x |> List.map(fun x -> { x = x; y = this.source.y }) |> Set
        else if (this.isPerfectDiagonal()) then 
            let yRange = getRange this.source.y this.dest.y
            let xRange = getRange this.source.x this.dest.x
            List.zip xRange yRange
            |> List.map(fun (x, y) -> { x = x; y = y })
            |> Set        
        else Set.empty
        
    member this.intersects(other: Line): Point list = 
        Set.intersect(this.coversPoints())(other.coversPoints()) 
        |> Set.toList


let rec pairs(lst: 'T list) = seq {
    match lst with 
    | h::t -> for e in t do yield (h, e)
              yield! pairs t
    | _ -> () }

let countOverlaps(lines: Line list): int =
    lines
    |> pairs
    |> Seq.collect(fun (l1, l2) -> l1.intersects(l2))
    |> Seq.distinct
    |> Seq.length

let parseLine(line: string): Line =
    let coords =
        line.Split([| '-'; '>'; ',' |], System.StringSplitOptions.TrimEntries)
        |> Array.filter(fun s -> 0 < s.Length)
        |> Array.map(int)
    match coords with
    | [| x1; y1; x2; y2 |] -> { source = { x = x1; y = y1 }; dest = { x = x2; y = y2 } }
    | _ -> invalidArg (nameof(line)) $"Line '{line}' is not of expected format int,int -> int,int"

let parseFile(lines: string list): Line list = lines |> List.map(parseLine)
    
let computeDay5Part1(): int =
    Utils.readInputFile("./Input/Day5.txt")
    |> parseFile
    |> List.filter(Line.isVerticalOrHorizontal)
    |> countOverlaps

let computeDay5Part2(): int =
    Utils.readInputFile("./Input/Day5.txt")
    |> parseFile
    |> List.filter(fun l -> l.isPerfectDiagonal() || l.isVerticalOrHorizontal())
    |> countOverlaps
    