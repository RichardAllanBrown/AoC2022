module Grid

open Point

type Grid = int[,]

module Grid =
    let get(h: Grid)(p: Point): int =
        Array2D.get(h)(p.x)(p.y)

    let getAllPoints(h: Grid): Point list =
        [ for x in 0 .. Array2D.length1(h) - 1 do
          for y in 0 .. Array2D.length2(h) - 1 do
          yield { x = x; y = y } ]

    let containsPoint(h: Grid)(p: Point): bool = 
        let xMax = Array2D.length1(h) - 1
        let yMax = Array2D.length2(h) - 1 
        0 <= p.x && p.x <= xMax && 0 <= p.y && p.y <= yMax

    let getCardinalNeighbouringPoints(h: Grid)(p: Point): Point list = 
        p 
        |> Point.getCardinalNeighbours 
        |> List.filter(containsPoint(h))

    let getSurroundingPoints(h: Grid)(p: Point): Point list = 
        p 
        |> Point.getAllNeighbours 
        |> List.filter(containsPoint(h))

    let map(f: int -> int)(h: Grid): Grid = Array2D.map(f)(h)

    let mapi(f: Point -> int -> int)(g: Grid): Grid = 
        Array2D.mapi(fun x -> fun y -> fun v -> f({x=x;y=y})(v))(g)

    let findPoints(f: int -> bool)(g: Grid): Point list =
        getAllPoints(g)
        |> List.map(fun p -> (p, get(g)(p)))
        |> List.filter(fun (_, v) -> f(v))
        |> List.map(fst)

    let parseFile(lines: string list): Grid =
        let digitLineToArr(str: string) = str.ToCharArray() |> Array.map(string) |> Array.map(int)
        lines |> List.map(digitLineToArr) |> array2D