module Day11

open Point
open Grid

let rec activateFlashes(alreadyFlashed: Point list)(grid: Grid): Grid =
    let flashers = grid |> Grid.findPoints(fun f -> 9 < f) |> List.filter(fun p -> not(List.contains(p)(alreadyFlashed)))
    if (List.isEmpty(flashers)) then grid
    else
        let neighbours = flashers |> List.collect(Grid.getSurroundingPoints(grid))
        grid
        |> Grid.mapi(fun p -> fun v -> v + (neighbours |> List.filter(fun i -> i = p) |> List.length))
        |> activateFlashes(List.append(alreadyFlashed)(flashers))

let stepOctopi(grid: Grid): Grid = 
    grid
    |> Grid.map(fun f -> f + 1)
    |> activateFlashes(List.empty)
    |> Grid.map(fun f -> if (9 < f) then 0 else f)

let countFlashes(grid: Grid): int =
    List.fold(fun (g, acc) -> fun _ -> 
        let newGrid = stepOctopi(g)
        (newGrid, acc + (Grid.findPoints(fun f -> f = 0)(newGrid)).Length)
    )(grid, 0)([1..100]) |> snd

let computeDay11Part1(): int =
    Utils.readInputFile("./Input/Day11.txt")
    |> Grid.parseFile
    |> countFlashes

let countDaysToFirstAllFlash(g: Grid): int =
    let rec helper(grids: Grid list): Grid list =
        let newGrid = stepOctopi(grids.Head)
        if ((Grid.findPoints(fun f -> f = 0)(newGrid)).Length = newGrid.Length) then newGrid :: grids
        else helper(newGrid :: grids)

    helper(List.singleton(g)).Length - 1

let computeDay11Part2(): int =
    Utils.readInputFile("./Input/Day11.txt")
    |> Grid.parseFile
    |> countDaysToFirstAllFlash
