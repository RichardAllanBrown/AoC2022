module Day9

open Point
open Grid

type Grid = int[,]

let isLowPoint(h: Grid)(location: Point): bool =
    let locationValue = Grid.get(h)(location)
    Grid.getCardinalNeighbouringPoints(h)(location)
    |> List.forall(fun p -> locationValue < Grid.get(h)(p))

let computeRiskLevel(h: Grid): int =
    Grid.getAllPoints(h)
    |> List.filter(isLowPoint(h))
    |> List.map(Grid.get(h))
    |> List.sumBy(fun x -> x + 1)

let computeDay9Part1(): int =
    Utils.readInputFile("./Input/Day9.txt")
    |> Grid.parseFile
    |> computeRiskLevel

let findBasinSize(h: Grid)(p: Point): int =
    let rec expandBasin(currentBasin: Point Set): Point Set =
        let result = 
            currentBasin 
            |> Set.toList 
            |> List.collect(Grid.getCardinalNeighbouringPoints(h))
            |> List.append(currentBasin |> Set.toList)
            |> List.filter(fun n -> Grid.get(h)(n) < 9)
            |> Set
        if (result = currentBasin) then result
        else expandBasin(result)

    p
    |> Set.singleton
    |> expandBasin
    |> Set.count

let multLargest3Basins(h: Grid): int =
    Grid.getAllPoints(h)
    |> List.filter(isLowPoint(h))
    |> List.map(findBasinSize(h))
    |> List.sortDescending
    |> List.take(3)
    |> List.fold(*)(1)

let computeDay9Part2(): int =
    Utils.readInputFile("./Input/Day9.txt")
    |> Grid.parseFile
    |> multLargest3Basins

