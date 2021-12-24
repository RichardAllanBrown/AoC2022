module Day13

open Point
open InputParsers

type Paper = Point list

module Paper =
    let getBounds(paper: Paper): int*int =
        let maxX = paper |> List.map(fun p -> p.x) |> List.max
        let maxY = paper |> List.map(fun p -> p.y) |> List.max
        ( maxX, maxY )

    let prettyString(paper: Paper): string =
        let (maxX, maxY) = getBounds(paper)
        let result = 
            [ 
                for x in 0 .. maxX do
                for y in 0 .. maxY do
                yield 
                    (if (List.contains({ x = x; y = y })(paper)) then "X" else " ") +
                    (if (y = maxY) then "/n" else "")
            ]
        System.String.Join("", result)

type Fold =
    | Vertical of x: int
    | Horizontal of y: int

module String =
    let NonEmpty(s: string) = 0 < s.Length

let applyFold(fold: Fold)(point: Point): Point =
    match fold with
    | Horizontal(y) -> { point with y = point.y - 2 * (point.y - y) }
    | Vertical(x) -> { point with x = point.x - 2 * (point.x - x) }

let foldPaper(paper: Paper)(fold: Fold): Paper = 
    let (stable, toTransform) = 
        match fold with 
        | Horizontal(y) -> List.partition(fun p -> p.y < y)(paper)
        | Vertical(x) -> List.partition(fun p -> p.x < x)(paper)    
    toTransform |> List.map(applyFold(fold)) |> List.append(stable) |> List.distinct

let parse(input: string list): Paper * Fold list =
    let unparsedMoves, unparsedPoints = input |> List.partition(fun s -> s.StartsWith("fold along"))
    let paper = 
        unparsedPoints 
        |> List.collect(function
            | ParseRegex "(\d+),(\d+)" [ Integer x; Integer y ] -> List.singleton { x = x; y = y }
            | _ -> List.empty)
    let folds = 
        unparsedMoves
        |> List.collect(function 
            | ParseRegex "fold along x=(\d+)" [ Integer x; ] -> List.singleton(Vertical(x = x))
            | ParseRegex "fold along y=(\d+)" [ Integer y; ] -> List.singleton(Horizontal(y = y))
            | _ -> List.empty)
    (paper, folds)

let computeDay13Part1(): int =
    let paper, folds = Utils.readInputFile("./Input/Day13.txt") |> parse
    foldPaper(paper)(folds.Head) |> List.length

let computeDay13Part2(): string =
    let paper, folds = Utils.readInputFile("./Input/Day13.txt") |> parse
    List.fold(foldPaper)(paper)(folds) |> Paper.prettyString
