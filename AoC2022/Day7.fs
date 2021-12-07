module Day7

let computeFuelNeeded(positions: int list)(targetPos: int): int =
    positions |> List.sumBy(fun p -> abs(p - targetPos))

let computeLeastCrabFuelNeeded(fuelFunc: int list -> int -> int)(positions: int list): int =
    [ List.min(positions) .. List.max(positions) ]
    |> List.map(fuelFunc(positions))
    |> List.min

let computeDay7Part1(): int = 
    Utils.readInputFile("./Input/Day7.txt")
    |> List.head
    |> Utils.splitDigits
    |> computeLeastCrabFuelNeeded(computeFuelNeeded)

let computeAccurateFuelNeeded(positions: int list)(targetPos: int): int =
    positions |> List.map(fun p -> abs(p - targetPos)) |> List.sumBy(fun f -> f*(f+1)/2)

let computeDay7Part2(): int = 
    Utils.readInputFile("./Input/Day7.txt")
    |> List.head
    |> Utils.splitDigits
    |> computeLeastCrabFuelNeeded(computeAccurateFuelNeeded)