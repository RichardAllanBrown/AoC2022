module Grid

open Point

type 'a Grid = 'a[,]

module Grid =
    let get(h: 'a Grid)(p: Point): 'a =
        Array2D.get(h)(p.x)(p.y)

    let getAllPoints(h: 'a Grid): Point list =
        [ for x in 0 .. Array2D.length1(h) - 1 do
          for y in 0 .. Array2D.length2(h) - 1 do
          yield { x = x; y = y } ]

    let containsPoint(h: 'a Grid)(p: Point): bool = 
        let xMax = Array2D.length1(h) - 1
        let yMax = Array2D.length2(h) - 1 
        0 <= p.x && p.x <= xMax && 0 <= p.y && p.y <= yMax

    let getCardinalNeighbouringPoints(h: 'a Grid)(p: Point): Point list = 
        p 
        |> Point.getCardinalNeighbours 
        |> List.filter(containsPoint(h))

    let getSurroundingPoints(h: 'a Grid)(p: Point): Point list = 
        p 
        |> Point.getAllNeighbours 
        |> List.filter(containsPoint(h))

    let map(f: 'a -> 'b)(h: 'a Grid): 'b Grid = Array2D.map(f)(h)

    let mapi(f: Point -> 'a -> 'b)(g: 'a Grid): 'b Grid = 
        Array2D.mapi(fun x -> fun y -> fun v -> f({x=x;y=y})(v))(g)

    let findPoints(f: 'a -> bool)(g: 'a Grid): Point list =
        getAllPoints(g)
        |> List.map(fun p -> (p, get(g)(p)))
        |> List.filter(fun (_, v) -> f(v))
        |> List.map(fst)

    let parseFile(lines: string list): int Grid =
        let digitLineToArr(str: string) = str.ToCharArray() |> Array.map(string) |> Array.map(int)
        lines |> List.map(digitLineToArr) |> array2D

    let copy = Array2D.copy

    let createSameSize(g: 'a Grid)(v: 'b): 'b Grid = Array2D.create(Array2D.length1(g))(Array2D.length2(g))(v)

    let set(g: 'a Grid)(p: Point)(v: 'a) = Array2D.set(g)(p.x)(p.y)(v)
