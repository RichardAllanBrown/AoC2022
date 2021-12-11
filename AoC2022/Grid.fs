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

    let getNeighbouringPoints(h: Grid)(p: Point): Point list = 
        p 
        |> Point.getCardinalNeighbours 
        |> List.filter(containsPoint(h))
