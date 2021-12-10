module Day9

type Point = 
    { x: int; y: int }

module Point =
    let getCardinalNeighbours(p: Point): Point list =
        [
            { x = p.x; y = p.y + 1 }
            { x = p.x; y = p.y - 1 }
            { x = p.x + 1; y = p.y }
            { x = p.x - 1; y = p.y }
        ]

type Heightmap = int[,]

module Heightmap =
    let get(h: Heightmap)(p: Point): int =
        Array2D.get(h)(p.x)(p.y)

    let getAllPoints(h: Heightmap): Point list =
        [ for x in 0 .. Array2D.length1(h) - 1 do
          for y in 0 .. Array2D.length2(h) - 1 do
          yield { x = x; y = y } ]

    let containsPoint(h: Heightmap)(p: Point): bool = 
        let xMax = Array2D.length1(h) - 1
        let yMax = Array2D.length2(h) - 1 
        0 <= p.x && p.x <= xMax && 0 <= p.y && p.y <= yMax

    let getNeighbouringPoints(h: Heightmap)(p: Point): Point list = 
        p 
        |> Point.getCardinalNeighbours 
        |> List.filter(containsPoint(h))

let isLowPoint(h: Heightmap)(location: Point): bool =
    let locationValue = Heightmap.get(h)(location)
    Heightmap.getNeighbouringPoints(h)(location)
    |> List.forall(fun p -> locationValue < Heightmap.get(h)(p))

let computeRiskLevel(h: Heightmap): int =
    Heightmap.getAllPoints(h)
    |> List.filter(isLowPoint(h))
    |> List.map(Heightmap.get(h))
    |> List.sumBy(fun x -> x + 1)

let parseFile(lines: string list): Heightmap =
    let digitLineToArr(str: string) = str.ToCharArray() |> Array.map(string) |> Array.map(int)
    lines |> List.map(digitLineToArr) |> array2D

let computeDay9Part1(): int =
    Utils.readInputFile("./Input/Day9.txt")
    |> parseFile
    |> computeRiskLevel

let findBasinSize(h: Heightmap)(p: Point): int =
    let rec expandBasin(currentBasin: Point Set): Point Set =
        let result = 
            currentBasin 
            |> Set.toList 
            |> List.collect(Heightmap.getNeighbouringPoints(h))
            |> List.append(currentBasin |> Set.toList)
            |> List.filter(fun n -> Heightmap.get(h)(n) < 9)
            |> Set
        if (result = currentBasin) then result
        else expandBasin(result)

    p
    |> Set.singleton
    |> expandBasin
    |> Set.count

let multLargest3Basins(h: Heightmap): int =
    Heightmap.getAllPoints(h)
    |> List.filter(isLowPoint(h))
    |> List.map(findBasinSize(h))
    |> List.sortDescending
    |> List.take(3)
    |> List.fold(*)(1)

let computeDay9Part2(): int =
    Utils.readInputFile("./Input/Day9.txt")
    |> parseFile
    |> multLargest3Basins

